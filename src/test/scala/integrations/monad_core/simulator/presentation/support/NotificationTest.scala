package integrations.monad_core.simulator.presentation.support

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import monad_core.simulator.presentation.components.NotificationManager
import org.scalatest.{BeforeAndAfterEach, Suite}
import scalafx.scene.Scene
import scalafx.scene.layout.StackPane
import scalafx.stage.Stage

import scala.compiletime.uninitialized

trait NotificationTest extends ScalaFxInit with SnapshotTesting with BeforeAndAfterEach:
  this: Suite =>
  protected var root: StackPane = uninitialized
  protected var stage: Stage = uninitialized

  override protected def beforeEach(): Unit =
    super.beforeEach()
    onFxThread {
      root = new StackPane() {
        prefWidth = 300
        prefHeight = 300
      }

      stage = new Stage() {
        scene = new Scene(root)
      }

      NotificationManager.attach(root)
      NotificationManager.animationsEnabled = true
    }

  override protected def afterEach(): Unit =
    onFxThread {
      NotificationManager.detach()
    }
    super.afterEach()