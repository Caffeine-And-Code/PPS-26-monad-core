package integrations.monad_core.simulator.presentation.support

import javafx.application.Platform
import monad_core.engine.errors.EngineError
import org.scalatest.{BeforeAndAfterAll, Suite}
import scalafx.scene.control.Button

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.annotation.tailrec

private[presentation] trait ScalaFxInit extends BeforeAndAfterAll:

  this: Suite =>
  override def beforeAll(): Unit = {
    ScalaFxToolkit.init()
    super.beforeAll()
  }

  def clickButton(button: Button, times: Int = 1): Unit =
    val latch = new CountDownLatch(1)

    @tailrec
    def buttonFireCycle(nRemainingTimes: Int): Unit =
      if nRemainingTimes > 0 then
        button.fire()
        buttonFireCycle(nRemainingTimes - 1)

    Platform.runLater: () =>
      buttonFireCycle(times)
      latch.countDown()

    val clicked = latch.await(5 * times, TimeUnit.SECONDS)
    assert(clicked, "Button click did not complete in time")

  def getOrFail[T](either: Either[EngineError, T]): T =
    either match
      case Right(value) => value
      case Left(err) => fail(s"Got error: $err")
