package quasimodo.expectation

import cats.effect.IO
import com.eed3si9n.expecty._
import cats.syntax.apply._

class ExpectyIO extends Recorder[Boolean, IO[Unit]] {

  class ExpectyListener extends RecorderListener[Boolean, IO[Unit]] {
    override def expressionRecorded(
        recordedExpr: RecordedExpression[Boolean],
        recordedMessage: Function0[String]
    ): Unit = {}

    override def recordingCompleted(
        recording: Recording[Boolean],
        recordedMessage: Function0[String]
    ): IO[Unit] = {
      recording.recordedExprs.foldLeft(IO.unit) {
        case (res, expr) =>
          lazy val rendering: String =
            new ExpressionRenderer(showTypes = false).render(expr)

          if (!expr.value) {
            val msg = recordedMessage()
            val header =
              "assertion failed" +
                (if (msg == "") ""
                 else ": " + msg)
            val fullMessage = header + "\n\n" + rendering
            res *> IO.raiseError(new AssertionError(fullMessage))
          } else IO.unit
      }
    }
  }

  override lazy val listener = new ExpectyListener
}
