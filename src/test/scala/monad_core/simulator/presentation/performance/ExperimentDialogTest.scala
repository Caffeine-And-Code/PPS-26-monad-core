package monad_core.simulator.presentation.performance

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

  test("a failed state displays its failure header and message"):
    val failureMessage = "failure message"
    val expectedContent = s"Performance test failed:\n$failureMessage"
    val (output, currentDialog) = onFxThread {
      val textArea = new TextArea()
      (textArea, Some(ResultDialogHandle(textArea)))
    }

    val result = onFxThread {
      ExperimentDialog.displayState(
        ExperimentState.Failed(failureMessage),
        owner = None,
        currentDialog = currentDialog
      )
    }

    onFxThread(output.text.value) shouldBe expectedContent
