package quasimodo.example

import cats.effect.Sync
import quasimodo.fixtures.{DependentFixture, UniRecorder}

object ExampleSpec {
  import Example._

  def main(args: Array[String]): Unit = {
    import cats.effect.IO
    import cats.syntax.applicative._
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    case class World[F[_]](
        serviceGet: F[DependentFixture[F, String, Int]],
        publish: F[UniRecorder[F, (String, Int)]],
        log: F[UniRecorder[F, (String, String)]]
    )

    case object Problem extends RuntimeException("fuuuuck")

    // Helper function that initialises fixtures from supplied World (needed to get the references into program)
    // and stashes results-only fixtures back into World
    def setup[F[_]: Sync](
        world: World[F]
    ): F[(Program[F], World[F])] =
      for {
        serviceGet <- world.serviceGet
        publish    <- world.publish
        log        <- world.log

        service   = testService(serviceGet)
        publisher = testPublish(publish)
        logger    = testLogger(log)

        // using the resultsOnly hack to re-use the same World class
        worldOfResults = World(
          serviceGet.resultsOnly.pure[F],
          publish.resultsOnly.pure[F],
          log.resultsOnly.pure[F]
        )

        program = new Program(service, publisher, logger)
      } yield (program, worldOfResults)

    val helpers = quasimodo.dsl.basic[IO]

    import helpers._

    // TESTS START HERE

    val worldOfPain: World[IO] = World(
      serviceGet = alwaysFailing[Int](Problem).map(_.ignoringInput[String]),
      publish = recordOnly[(String, Int)],
      log = recordOnly[(String, String)]
    )

    def expect = new quasimodo.expectation.ExpectyIO

    val FailWhenServiceFails = setup(worldOfPain).flatMap {
      case (program, worldResults) =>
        for {
          result <- program.run(List("b")).attempt

          serviceGets  <- worldResults.serviceGet.flatMap(_.results)
          publishCalls <- worldResults.publish.flatMap(_.results)
          logs         <- worldResults.log.flatMap(_.results)

          // assertions can happen here
          _ <- expect(result.isLeft)
          _ <- expect(serviceGets == List(Left(Problem)))
          _ <- expect(publishCalls.isEmpty)
          _ <- expect(logs.isEmpty)
        } yield ()
    }

    val SucceedWhenServiceSucceeds = setup(
      worldOfPain.copy(serviceGet = alwaysReturning(5).map(_.ignoringInput[String]))
    ).flatMap {
      case (program, worldResults) =>
        for {
          result <- program.run(List("b")).attempt

          serviceGets  <- worldResults.serviceGet.flatMap(_.results)
          publishCalls <- worldResults.publish.flatMap(_.results)
          logs         <- worldResults.log.flatMap(_.results)

          // assertions can happen here
          _ <- expect(result.isRight)
          _ <- expect(serviceGets == List(Right(5)))
          _ <- expect(publishCalls == List(Right("b" -> 5)))
          _ <- expect(logs == List(Right("INFO" -> s"Received 5 for b")))
        } yield ()
    }

    val SucceedDifferentlyDependingOnInput = setup(
      worldOfPain.copy(
        serviceGet = givenThen(
          "b" -> IO.pure(5),
          "a" -> IO.pure(6)
        )
      )
    ).flatMap {
      case (program, worldResults) =>
        for {
          result <- program.run(List("b", "a")).attempt

          serviceGets  <- worldResults.serviceGet.flatMap(_.results)
          publishCalls <- worldResults.publish.flatMap(_.results)
          logs         <- worldResults.log.flatMap(_.results)

          // assertions can happen here
          _ <- expect(result.isRight)
          _ <- expect(serviceGets == List(Right(5), Right(6)))
          _ <- expect(publishCalls == List(Right("b" -> 5), Right("a" -> 6)))
          _ <- expect(
            logs == List(Right("INFO" -> "Received 5 for b"), Right("INFO" -> "Received 6 for a"))
          )
        } yield ()
    }

    val suite = for {
      _ <- FailWhenServiceFails
      _ <- SucceedWhenServiceSucceeds
      _ <- SucceedDifferentlyDependingOnInput
    } yield ()

    suite.attempt.unsafeRunSync() match {
      case Right(_) => println("All tests succeded")
      case Left(ex) => println("Failures in tests found"); throw ex
    }
  }

}
