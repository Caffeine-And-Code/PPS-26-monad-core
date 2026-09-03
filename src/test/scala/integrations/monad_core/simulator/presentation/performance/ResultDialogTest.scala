package integrations.monad_core.simulator.presentation.performance

import integrations.monad_core.simulator.presentation.support.DialogTesting
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import javafx.scene.control.{Button, TextArea}
import monad_core.simulator.CannotBuildDialog
import monad_core.simulator.presentation.performance.ResultDialog
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.{jfxNode2sfx, jfxStage2sfx}

class ResultDialogTest extends AnyFunSuite with Matchers with Inside with DialogTesting:

  private val Report =
    """Performance experiment: Stress
      |Entities: 100
      |p50: 1.000 ms
      |p95: 2.000 ms
      |p99: 3.000 ms
      |Frame budget completion: 95.00%""".stripMargin

  private val UpdatedReport = "Performance experiment: Load"

  private def output: TextArea =
    getRequiredActiveStage.getScene.getRoot
      .lookup(".performance-result-output")
      .asInstanceOf[TextArea]

  private def closeButton: Button =
    getRequiredActiveStage.getScene.getRoot
      .lookup(".performance-result-close")
      .asInstanceOf[Button]

  test("show opens the performance result dialog"):
    val result = onFxThread {
      ResultDialog.show(Report)
    }

    result shouldBe Right(())

  test("the result dialog uses its public title"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    onFxThread(getRequiredActiveStage.getTitle) shouldBe ResultDialog.Title

  test("the result dialog displays its content"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    onFxThread(output.getText) shouldBe Report

  test("the result output is read-only"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    onFxThread(output.isEditable) shouldBe false

  test("the result output does not wrap its content"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    onFxThread(output.isWrapText) shouldBe false

  test("the result dialog is resizable"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    onFxThread(getRequiredActiveStage.isResizable) shouldBe true

  test("the result handle updates its content"):
    val dialog = onFxThread {
      getOrFail(ResultDialog.open(Report))
    }

    onFxThread {
      dialog.update(UpdatedReport)
    }

    onFxThread(output.getText) shouldBe UpdatedReport

  test("the close control uses an explicit label"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    onFxThread(closeButton.getText) shouldBe "Close"

  test("the close control closes the result dialog"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }
    val stage = getRequiredActiveStage

    onFxThread {
      closeButton.fire()
    }

    onFxThread(stage.isShowing) shouldBe false

  test("show translates a graphical-thread violation"):
    val result = ResultDialog.show(Report)

    inside(result):
      case Left(error: CannotBuildDialog) =>
        error.dialogId shouldBe "PerformanceResultDialog"

  test("the performance result matches its architectural snapshot"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    val stage = onFxThread(getRequiredActiveStage)

    assertMatchesArchitecturalSnapshotOfStage(
      "performance_test_result",
      stage
    )

  test("the performance result matches its visual snapshot"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
    }

    val root: scalafx.scene.Node = onFxThread(getRequiredActiveStage.getScene.getRoot)

    assertMatchesVisualSnapshot(
      "performance_test_result",
      root,
      maxDiffPercentage = 8.0
    )
