package integrations.monad_core.performance.presentation.gui

import integrations.monad_core.simulator.presentation.support.DialogTesting
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import javafx.scene.control.{Button, TextArea}
import monad_core.performance.presentation.gui.ResultDialog
import monad_core.simulator.CannotBuildDialog
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.jfxNode2sfx

class ResultDialogTest extends AnyFunSuite with Matchers with Inside with DialogTesting:

  private val Report =
    """Performance experiment: Stress
      |Entities: 100
      |p50: 1.000 ms
      |p95: 2.000 ms
      |p99: 3.000 ms
      |Frame budget completion: 95.00%""".stripMargin

  private val UpdatedReport = "Performance experiment: Load"

  test("the performance result dialog can be shown"):
    onFxThread {
      val result = ResultDialog.show(Report)

      result shouldBe Right(())
    }

  test("the performance result dialog displays a read-only report"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
      val output = getRequiredActiveStage.getScene.getRoot
        .lookup(".performance-result-output")
        .asInstanceOf[TextArea]

      output.getText shouldBe Report
      output.isEditable shouldBe false
    }

  test("an open performance result dialog can update its displayed content"):
    onFxThread {
      val dialog = getOrFail(ResultDialog.open(Report))

      dialog.update(UpdatedReport)

      val output = getRequiredActiveStage.getScene.getRoot
        .lookup(".performance-result-output")
        .asInstanceOf[TextArea]
      output.getText shouldBe UpdatedReport
    }

  test("the performance result dialog closes from its close button"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
      val stage = getRequiredActiveStage
      val closeButton = stage.getScene.getRoot
        .lookup(".performance-result-close")
        .asInstanceOf[Button]

      closeButton.fire()

      stage.isShowing shouldBe false
    }

  test("the performance result dialog matches its visual snapshot"):
    onFxThread {
      getOrFail(ResultDialog.show(Report))
      val rootNode: scalafx.scene.Node = getRequiredActiveStage.getScene.getRoot

      assertMatchesVisualSnapshot(
        "performance_test_result",
        rootNode,
        maxDiffPercentage = 8.0
      )
    }

  test("the performance result dialog wraps a JavaFX thread violation"):
    val result = ResultDialog.show(Report)

    inside(result):
      case Left(error: CannotBuildDialog) =>
        error.dialogId shouldBe "PerformanceResultDialog"
