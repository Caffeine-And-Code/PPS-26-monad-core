package integrations.monad_core.simulator.presentation.panels.support

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.SceneGraphSerializer.NodeSnapshot
import integrations.monad_core.simulator.presentation.support.{
  NotificationTest,
  SceneGraphSerializer
}
import monad_core.simulator.TeamNotFoundDuringSelection
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.panels.support.FormUtilities
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.layout.StackPane

class FormUtilitiesTest extends AnyFunSuite with NotificationTest with Matchers with MockFactory:

  test(
    "displayError display a notification message on the Stage where the NotificationManager is attached"
  ):
    val expectedMessage = "An Error has Occurred!"
    val error           = new BaseError(expectedMessage) {}

    onFxThread {
      FormUtilities.displayError(error)
    }

    val sceneSnapshot: NodeSnapshot = SceneGraphSerializer.snapshotOf(root)

    sceneSnapshot.children.head.text should be(Some(expectedMessage))
