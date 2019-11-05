package quasimodo.example

import cats.{MonadError, Traverse}
import quasimodo.fixtures.{DependentProducer, UniRecorder}

object Example {

  // test algebras
  trait Service[F[_]] {
    def get(a: String): F[Int]
  }

  trait Publish[F[_]] {
    def publish(t: (String, Int)): F[Unit]
  }

  trait Logger[F[_]] {
    def log(level: String, msg: String): F[Unit]
  }

  // test program
  type Input = List[String]

  class Program[F[_]](
      service: Service[F],
      publish: Publish[F],
      logger: Logger[F]
  )(implicit F: MonadError[F, Throwable]) {

    import cats.instances.list._
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def process(i: String) =
      for {
        s <- service.get(i)
        _ <- logger.log("INFO", s"Received $s for $i")
        _ <- publish.publish(i -> s)
      } yield ()

    def run(i: Input): F[Unit] = Traverse[List].traverse(i)(process).void
  }

  // TEST Algebra definitions

  def testService[F[_]](getFixture: DependentProducer[F, String, Int]): Service[F] =
    new Service[F] {
      def get(a: String): F[Int] = getFixture.produce(a)
    }

  def testPublish[F[_]](publishFixture: UniRecorder[F, (String, Int)]): Publish[F] =
    new Publish[F] {
      def publish(t: (String, Int)): F[Unit] = publishFixture.recordValue(t)
    }

  def testLogger[F[_]](logFixture: UniRecorder[F, (String, String)]): Logger[F] = new Logger[F] {
    def log(level: String, msg: String): F[Unit] =
      logFixture.recordValue(level -> msg)
  }

}
