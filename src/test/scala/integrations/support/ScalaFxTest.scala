package integrations.support

import javafx.application.Platform
import javafx.scene.{Node, Parent}
import org.scalatest.{BeforeAndAfterAll, Suite}

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.jdk.CollectionConverters.*

trait ScalaFxTest extends BeforeAndAfterAll:
  this: Suite =>

  private val fxTimeoutSeconds = 5L

  override protected def beforeAll(): Unit =
    super.beforeAll()
    val started = new CountDownLatch(1)
    try Platform.startup(() => started.countDown())
    catch case _: IllegalStateException => started.countDown()
    await(started, "JavaFX runtime did not start")

  protected def onFxThread[A](action: => A): A =
    if Platform.isFxApplicationThread then action
    else
      val completed = new CountDownLatch(1)
      @volatile var result: Either[Throwable, A] = null
      Platform.runLater { () =>
        result =
          try Right(action)
          catch case error: Throwable => Left(error)
        completed.countDown()
      }
      await(completed, "JavaFX action did not complete")
      result.fold(throw _, identity)

  protected def drainFxQueue(): Unit = onFxThread(())

  protected def descendants(node: Node): Seq[Node] =
    node +: (node match
      case scrollPane: javafx.scene.control.ScrollPane => descendants(scrollPane.getContent)
      case parent: Parent => parent.getChildrenUnmodifiable.asScala.toSeq.flatMap(descendants)
      case _              => Seq.empty)

  private def await(latch: CountDownLatch, failureMessage: String): Unit =
    if !latch.await(fxTimeoutSeconds, TimeUnit.SECONDS) then
      throw new AssertionError(failureMessage)
