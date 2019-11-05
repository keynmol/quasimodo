package quasimodo.example

import cats.effect.Sync
import org.scalacheck.{Gen, Prop}
import quasimodo.fixtures.{DependentFixture, UniRecorder}

object ExamplePropertySpec {
  import Example._

  def main(args: Array[String]): Unit = {
    import cats.effect.IO
    import cats.syntax.applicative._
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    // There's no magic surrounding a class like this,
    // you might as well not use a container class at all if all you have is one
    // fixture
    case class World[F[_]](
        serviceGet: F[DependentFixture[F, String, Int]],
        publish: F[UniRecorder[F, (String, Int)]],
        log: F[UniRecorder[F, (String, String)]]
    )

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

    val basicDSL = quasimodo.dsl.basic[IO]
    import basicDSL._

    val propertyTestingDSL = quasimodo.dsl.propertyTesting[IO]
    import propertyTestingDSL._

    // TESTS START HERE

    val DEFAULT_SERVICE_VALUE = 5
    // An always succeeding world, used as a starting point
    val defaultWorld: World[IO] = World(
      serviceGet = always[Int](5).map(_.ignoringInput[String]),
      publish = recordOnly[(String, Int)],
      log = recordOnly[(String, String)]
    )

    val inputGen = Gen.listOf(Gen.oneOf("a", "b")).label("input")

    // TEST 1: happy path
    val happyPath = Prop.forAllNoShrink(
      inputGen
    ) {
      case (input) =>
        setup(defaultWorld).flatMap {
          case (program, worldResults) =>
            for {
              // Run the program
              result <- program.run(input).attempt

              serviceGets  <- worldResults.serviceGet.flatMap(_.results)
              publishCalls <- worldResults.publish.flatMap(_.results)
              logMessages  <- worldResults.log.flatMap(_.results)

              _ <- expect(result.isRight)

              expectedServiceCalls = input.map(_ => Right(DEFAULT_SERVICE_VALUE))
              _ <- expect(serviceGets == expectedServiceCalls)

              expectedPublishCalls = input.map(s => Right(s -> DEFAULT_SERVICE_VALUE))
              _ <- expect(publishCalls == expectedPublishCalls)

              expectedLogMessages = input.map(
                s => Right("INFO" -> s"Received $DEFAULT_SERVICE_VALUE for $s")
              )
              _ <- expect(logMessages == expectedLogMessages)
            } yield ()
        }
    }

    val flakyServiceGet: Gen[IO[Int]] =
      Gen.oneOf(
        Gen.posNum[Int].map(IO.pure),
        Gen.alphaChar.map(c => IO.raiseError(new RuntimeException(s"generated error $c")))
      )

    // Test 2: flaky service get short-circuits rest of calls
    val flakyPath = Prop.forAllNoShrink(
      generatedF(flakyServiceGet).producerGen("serviceGet"),
      inputGen
    ) {
      case (getFixture, input) =>
        val newWorld = defaultWorld.copy(
          serviceGet = always(getFixture).map(_.ignoringInput[String])
        )

        setup(newWorld).flatMap {
          case (program, worldResults) =>
            for {
              // Run the program
              result <- program.run(input).attempt

              serviceGets  <- worldResults.serviceGet.flatMap(_.results)
              publishCalls <- worldResults.publish.flatMap(_.results)
              logMessages  <- worldResults.log.flatMap(_.results)

              // tests
              _ <- if (result.isLeft)
                expect(publishCalls.length < input.length && logMessages.length < input.length)
              else
                expect {
                  publishCalls.length == input.length &&
                  logMessages.length == input.length &&
                  serviceGets.length == input.length
                }
            } yield ()
        }
    }

    happyPath.label("Happy path").check()
    flakyPath.label("Flaky service gets").check()

  }

}
