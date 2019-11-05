package quasimodo.dsl

import cats.effect.Sync
import org.scalacheck.Gen
import quasimodo.fixtures.{
  AutoDependentFixture,
  AutoFixture,
  ConditionalProducer,
  ConstProducer,
  DependentFixture,
  DependentProducer,
  GenProducer,
  Sink,
  UniFixture,
  UniRecorder
}

class basic[F[_]: Sync] {
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def alwaysReturning[A](v: A): F[UniFixture[F, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- GenProducer(Gen.const(v.pure[F])).pure[F]
    } yield AutoFixture.of(UniFixture.of(producer, recorder))
  }

  def givenThen[I, A](values: (I, F[A])*): F[DependentFixture[F, I, A]] =
    givenThenProduce(values.map {
      case (trigger, valueF) =>
        trigger -> ConstProducer(valueF).ignoringInput[I]
    }: _*)

  def givenThenProduce[I, A](
      values: (I, DependentProducer[F, I, A])*
  ): F[DependentFixture[F, I, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- ConditionalProducer(values.toMap).pure[F]
    } yield AutoDependentFixture.of(DependentFixture.of(producer, recorder))
  }

  def alwaysF[A](v: F[A]): F[UniFixture[F, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- ConstProducer(v).pure[F]
    } yield AutoFixture.of(UniFixture.of(producer, recorder))
  }

  def always[A](v: Either[Throwable, A]): F[UniFixture[F, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- ConstProducer(Sync[F].fromEither(v)).pure[F]
    } yield AutoFixture.of(UniFixture.of(producer, recorder))
  }

  def always[A](v: A): F[UniFixture[F, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- ConstProducer(v.pure[F]).pure[F]
    } yield AutoFixture.of(UniFixture.of(producer, recorder))
  }

  def alwaysFailing[A](exception: Throwable): F[UniFixture[F, A]] = {
    for {
      recorder <- Sink.apply[F, A]
      producer <- ConstProducer(exception.raiseError[F, A]).pure[F]
    } yield AutoFixture.of(UniFixture.of(producer, recorder))

  }

  def recordOnly[A]: F[UniRecorder[F, A]] =
    Sink.apply[F, A]

  def expect = new quasimodo.expectation.ExpectyIO
}

object basic {
  def apply[F[_]: Sync] = new basic[F]
}
