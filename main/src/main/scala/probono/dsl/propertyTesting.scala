package quasimodo.dsl

import cats.effect.{IO, Sync}
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{Exception, Result}
import quasimodo.fixtures._

class propertyTesting[F[_]: Sync] {

  import cats.syntax.all._

  def producerToGenF[A](p: IO[UniFixture[IO, A]], label: String): Gen[Either[Throwable, A]] = {
    Gen
      .posNum[Int]
      .map(_ => p.flatMap(_.produce).attempt.unsafeRunSync())
      .label(label)
  }

  def producerToGen[A](p: UniFixture[IO, A], label: String): Gen[Either[Throwable, A]] = {
    Gen.posNum[Int].map(_ => p.produce.attempt.unsafeRunSync()).label(label)
  }

  def generated[A](gen: Gen[A]): F[UniFixture[F, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- GenProducer(gen.map(_.pure[F])).pure[F]
    } yield AutoFixture.of(UniFixture.of(producer, recorder))
  }

  def generatedF[A](gen: Gen[F[A]]): F[UniFixture[F, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- GenProducer(gen).pure[F]
    } yield AutoFixture.of(UniFixture.of(producer, recorder))

  }

  def flakyRecordOnly[A](errorPercent: Int, error: Throwable): F[UniFixture[F, A]] =
    for {
      baseRecorder <- Sink.apply[F, A]
      producer     <- ErrorProducer[F, A].pure[F]

      successPercent = 100 - errorPercent.min(100)

      generator = Gen.frequency[F[Unit]](
        successPercent -> Gen.const(().pure[F]),
        errorPercent   -> Gen.const(error.raiseError[F, Unit])
      )
    } yield UniFixture.of(producer, GenRecorder(baseRecorder, generator))

  implicit class UniFixtureOps[A](fixture: IO[UniFixture[IO, A]]) {
    def producerGen(label: String): Gen[Either[Throwable, A]] =
      producerToGenF(fixture, label)
  }

  implicit val toProp: IO[Unit] => Prop = { result =>
    result.attempt.unsafeRunSync() match {
      case Right(()) => Prop.passed
      case Left(ex)  => Prop.apply(Result(status = Exception(ex)))
    }
  }

}

object propertyTesting {
  def apply[F[_]: Sync] = new propertyTesting[F]

}
