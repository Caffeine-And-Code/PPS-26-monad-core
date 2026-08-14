package integrations.monad_core.simulator.presentation.support

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import javafx.application.Platform
import javafx.scene.{Node, Parent}
import javafx.scene.control.ContextMenu as JfxContextMenu
import javafx.stage.{Stage, Window}
import monad_core.engine.errors.EngineError
import monad_core.simulator.errors.BaseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, Suite}
import scalafx.scene.control.Button

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

private[simulator] trait ScalaFxInit extends BeforeAndAfterAll with Matchers:
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
    clicked should be(true)

  def tryGetMainWindow: Option[Stage] =
    Window.getWindows.asScala.collectFirst {
      case stage: javafx.stage.Stage if stage.getTitle == "MonadCore2D" => stage
    }

  def getRequiredActiveStage: javafx.stage.Stage =
    getActiveStage
      .getOrElse(fail("No active showing stage found"))

  def getActiveStage: Option[javafx.stage.Stage] =
    Window.getWindows.asScala
      .collectFirst { case s: javafx.stage.Stage if s.isShowing => s }

  def findOpenContextMenu(): Option[JfxContextMenu] =
    Window.getWindows.asScala.collectFirst { case cm: JfxContextMenu => cm }

  def getOrFail[T](either: Either[BaseError | EngineError, T]): T =
    either match
      case Right(value) => value
      case Left(err)    => fail(s"Got error: $err")

  protected def drainFxQueue(): Unit = onFxThread(())

  protected def descendants(node: Node): Seq[Node] =
    node +: (node match
      case scrollPane: javafx.scene.control.ScrollPane => descendants(scrollPane.getContent)
      case parent: Parent => parent.getChildrenUnmodifiable.asScala.toSeq.flatMap(descendants)
      case _              => Seq.empty
    )
