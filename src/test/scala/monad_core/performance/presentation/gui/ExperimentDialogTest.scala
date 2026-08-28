package monad_core.performance.presentation.gui

import integrations.monad_core.simulator.presentation.support.DialogTesting
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import monad_core.simulator.presentation.components.NotificationManager
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.control.TextArea

class ExperimentDialogTest extends AnyFunSuite with Matchers with DialogTesting:

  test("the ready state preserves the current result dialog"):
    val currentDialog = onFxThread {
      Some(ResultDialogHandle(new TextArea()))
    }

    val result = ExperimentDialog.displayState(
      ExperimentState.Ready,
      owner = None,
      currentDialog = currentDialog
    )

    result shouldBe currentDialog

  test("a result dialog construction failure leaves no reusable dialog"):
    NotificationManager.detach()

    val result = ExperimentDialog.displayState(
      ExperimentState.Running,
      owner = None,
      currentDialog = None
    )

    result shouldBe None
