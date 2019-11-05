package quasimodo.fixtures

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.{ApplicativeError, MonadError}
import org.scalacheck.Gen
import quasimodo.fixtures.types._

case object ResultsOnlyFailure
    extends RuntimeException(
      "This is a results-only fixture - it cannot be used for recording or producing values"
    )

trait UniRecorder[F[_], A] {
  def record(value: Either[Throwable, A]): F[Unit]
  def recordValue(t: A): F[Unit] = record(Right(t))
  def results: F[RecordedResults[A]]

  def resultsOnly(implicit A: ApplicativeError[F, Throwable]): UniRecorder[F, A] = {
    val fixture = this
    new UniRecorder[F, A] {
      override def record(value: Either[Throwable, A]): F[Unit] =
        A.raiseError(ResultsOnlyFailure)

      override def results: F[RecordedResults[A]] = fixture.results
    }
  }
}

trait DependentRecorder[F[_], I, A] extends UniRecorder[F, (I, A)]

trait DependentProducer[F[_], I, A] {
  def produce(i: I): F[A]
}

trait UniProducer[F[_], A] extends DependentProducer[F, Any, A] {
  def produce: F[A]
  def produce(i: Any): F[A] = produce

  def ignoringInput[I]: DependentProducer[F, I, A] = {
    val self = this
    new DependentProducer[F, I, A] {
      override def produce(i: I): F[A] = self.produce
    }
  }
}

trait DependentFixture[F[_], I, A] extends UniRecorder[F, A] with DependentProducer[F, I, A] {

  override def resultsOnly(
      implicit A: ApplicativeError[F, Throwable]
  ): DependentFixture[F, I, A] = {
    val fixture = this
    new DependentFixture[F, I, A] {
      override def record(value: Either[Throwable, A]): F[Unit] =
        A.raiseError(ResultsOnlyFailure)

      override def results: F[RecordedResults[A]] = fixture.results

      override def produce(i: I): F[A] = A.raiseError(ResultsOnlyFailure)
    }
  }
}

object DependentFixture {
  def of[F[_], I, A](producer: DependentProducer[F, I, A], recorder: UniRecorder[F, A]) =
    new DependentFixture[F, I, A] {
      override def produce(i: I): F[A] = producer.produce(i)

      override def record(value: Either[Throwable, A]): F[Unit] =
        recorder.record(value)

      override def results: F[RecordedResults[A]] = recorder.results
    }
}

trait UniFixture[F[_], A] extends UniRecorder[F, A] with UniProducer[F, A] {
  override def produce: F[A]
  def asRecorder: UniRecorder[F, A] = this
  def asProducer: UniProducer[F, A] = this

  override def ignoringInput[I]: DependentFixture[F, I, A] = {
    val self = this
    new DependentFixture[F, I, A] {
      override def produce(i: I): F[A] = self.produce

      override def record(value: Either[Throwable, A]): F[Unit] =
        self.record(value)

      override def results: F[RecordedResults[A]] = self.results
    }
  }
}

object UniFixture {
  def of[F[_], A](producer: UniProducer[F, A], recorder: UniRecorder[F, A]): UniFixture[F, A] =
    new UniFixture[F, A] {
      def produce: F[A]                                = producer.produce
      def record(value: Either[Throwable, A]): F[Unit] = recorder.record(value)
      def results: F[RecordedResults[A]]               = recorder.results
    }

  implicit class UniFixtureOps[F[_], A](fixture: UniFixture[F, A]) {
    def ignoringInput[I]: DependentFixture[F, I, A] =
      new DependentFixture[F, I, A] {
        override def produce(i: I) = fixture.produce
        override def record(a: Either[Throwable, A]): F[Unit] =
          fixture.record(a)
        override def results = fixture.results
      }
  }
}

class ErrorProducer[F[_], A](implicit F: ApplicativeError[F, Throwable]) extends UniProducer[F, A] {
  def produce: F[A] = F.raiseError[A](GeneratorExhausted)
}

object ErrorProducer {
  def apply[F[_], A](implicit F: ApplicativeError[F, Throwable]) =
    new ErrorProducer[F, A]
}

case class Sink[F[_], A](ref: Ref[F, RecordedResults[A]])(implicit F: MonadError[F, Throwable])
    extends UniRecorder[F, A] {
  import cats.syntax.functor._

  override def record(value: Either[Throwable, A]): F[Unit] = {
    for {
      _ <- ref.update(_ ++ List(value))
    } yield ()
  }

  override def results: F[RecordedResults[A]] = ref.get
}

object Sink {
  import cats.syntax.functor._

  def apply[F[_]: Sync, A]: F[UniRecorder[F, A]] =
    Ref.of[F, RecordedResults[A]](List.empty[Either[Throwable, A]]).map(Sink(_))
}

case object GeneratorExhausted
    extends RuntimeException("Generator provided in the fixture was exhausted prematurely!")

case class AutoFixture[F[_], A] private (base: UniFixture[F, A], reRaise: Throwable => Boolean)(
    implicit x: MonadError[F, Throwable]
) extends UniFixture[F, A] {
  import cats.syntax.applicativeError._
  import cats.syntax.apply._
  import cats.syntax.flatMap._

  override def produce: F[A] = base.produce.attempt.flatMap {
    case Left(ex) if reRaise(ex)  => x.raiseError[A](ex)
    case Left(ex) if !reRaise(ex) => base.record(Left(ex)) *> x.raiseError(ex)
    case Right(value)             => base.record(Right(value)) *> x.pure(value)
  }

  override def record(value: Either[Throwable, A]): F[Unit] = base.record(value)

  override def results: F[RecordedResults[A]] = base.results
}

case class AutoDependentFixture[F[_], I, A] private (
    base: DependentFixture[F, I, A],
    reRaise: Throwable => Boolean
)(implicit x: MonadError[F, Throwable])
    extends DependentFixture[F, I, A] {
  import cats.syntax.applicativeError._
  import cats.syntax.apply._
  import cats.syntax.flatMap._

  override def produce(i: I): F[A] = base.produce(i).attempt.flatMap {
    case Left(ex) if reRaise(ex)  => x.raiseError[A](ex)
    case Left(ex) if !reRaise(ex) => base.record(Left(ex)) *> x.raiseError(ex)
    case Right(value)             => base.record(Right(value)) *> x.pure(value)
  }

  override def record(value: Either[Throwable, A]): F[Unit] = base.record(value)

  override def results: F[RecordedResults[A]] = base.results
}
object AutoDependentFixture {
  def of[F[_], I, A](fixture: DependentFixture[F, I, A])(implicit M: MonadError[F, Throwable]) =
    new AutoDependentFixture(fixture, _ => false)

}

object AutoFixture {
  def of[F[_], A](fixture: UniFixture[F, A])(implicit M: MonadError[F, Throwable]) =
    new AutoFixture(fixture, _ => false)
}

case class GenProducer[F[_], A](gen: Gen[F[A]])(implicit F: ApplicativeError[F, Throwable])
    extends UniProducer[F, A] {
  override def produce: F[A] = gen.sample match {
    case None        => F.raiseError(GeneratorExhausted)
    case Some(value) => value
  }
}

case class ConditionalGenProducer[F[_], I, A](values: Map[I, Gen[F[A]]])(
    implicit F: ApplicativeError[F, Throwable]
) extends DependentProducer[F, I, A] {
  val producers: Map[I, DependentProducer[F, I, A]] =
    values.map {
      case (trigger, generator) =>
        trigger -> GenProducer(generator).ignoringInput[I]
    }

  val base = ConditionalProducer(producers)

  override def produce(i: I): F[A] =
    base.produce(i)
}

case class ConstProducer[F[_], A](value: F[A]) extends UniProducer[F, A] {
  override def produce: F[A] = value
}

case class NoMatchingValue[I](value: I)
    extends RuntimeException(s"Unexpected value: fixture not setup for value $value")

case class ConditionalProducer[F[_], I, A](values: Map[I, DependentProducer[F, I, A]])(
    implicit F: ApplicativeError[F, Throwable]
) extends DependentProducer[F, I, A] {
  override def produce(i: I): F[A] =
    values.get(i).fold[F[A]](F.raiseError(NoMatchingValue(i)))(_.produce(i))
}

import cats.ApplicativeError
import cats.syntax.apply._
import org.scalacheck.Gen

case class GenRecorder[F[_], A](base: UniRecorder[F, A], gen: Gen[F[Unit]])(
    implicit F: ApplicativeError[F, Throwable]
) extends UniRecorder[F, A] {
  override def record(value: Either[Throwable, A]): F[Unit] = gen.sample match {
    case None         => F.raiseError(GeneratorExhausted)
    case Some(result) => result *> base.record(value)
  }
  override def results: F[RecordedResults[A]] = base.results
}
