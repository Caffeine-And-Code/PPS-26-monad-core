package integrations.monad_core.simulator.presentation.performance

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import javafx.scene.control.TextArea
import javafx.stage.{Stage, Window}
import monad_core.performance.model.{InvalidPerformanceArgument, PerformanceError}
import monad_core.performance.simulator.PerformanceCli
import monad_core.simulator.presentation.performance.{
  ExperimentCommand,
  ExperimentDialog,
  ResultDialog
}
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.{jfxNode2sfx, jfxStage2sfx}

import scala.concurrent.{Future, Promise}
import scala.jdk.CollectionConverters.*

class ExperimentDialogTest extends AnyFunSuite with Matchers with DialogTesting with FormTesting:

  private def pendingRunner: (
      ExperimentDialog.RunExperiment,
      Promise[Either[PerformanceError, String]]
  ) =
    val result = Promise[Either[PerformanceError, String]]()
    (_ => result.future, result)

  private def resultStage: Option[Stage] =
    Window.getWindows.asScala.collectFirst {
      case stage: Stage if stage.isShowing && stage.getTitle == ExperimentDialog.ResultTitle =>
        stage
    }

  private def resultOutput: TextArea =
    resultStage.value.getScene.getRoot
      .lookup(".performance-result-output")
      .asInstanceOf[TextArea]

  private def open(runner: ExperimentDialog.RunExperiment): Unit =
    onFxThread {
      getOrFail(ExperimentDialog.show(runner))
    }

  private def submit(): Unit =
    onFxThread {
      formSaveButton.fire()
    }

  test("show opens the performance form"):
    val (runner, _) = pendingRunner

    open(runner)

    onFxThread(getRequiredActiveStage.getTitle) shouldBe ExperimentDialog.Title

  test("the performance form uses an explicit run label"):
    val (runner, _) = pendingRunner

    open(runner)

    onFxThread(formSaveButton.getText) shouldBe "Run"

  test("submit immediately displays the running message"):
    val (runner, _) = pendingRunner
    open(runner)

    submit()

    onFxThread(resultOutput.getText) shouldBe "Performance test running..."

  test("submit executes the selected performance route"):
    var receivedCommand = Option.empty[ExperimentCommand]
    val runner: ExperimentDialog.RunExperiment = command =>
      receivedCommand = Some(command)
      Future.successful(Right("report"))
    open(runner)

    submit()

    receivedCommand.value.route shouldBe PerformanceCli.StressRoute

  test("submit includes the common form arguments"):
    var receivedCommand = Option.empty[ExperimentCommand]
    val runner: ExperimentDialog.RunExperiment = command =>
      receivedCommand = Some(command)
      Future.successful(Right("report"))
    open(runner)

    submit()

    receivedCommand.value.arguments should contain(PerformanceCli.Entities)

  test("an invalid selection does not execute the experiment"):
    var executions = 0
    val runner: ExperimentDialog.RunExperiment = _ =>
      executions += 1
      Future.successful(Right("report"))
    open(runner)
    onFxThread {
      allFormComboBoxes.head.setValue("Unknown")
    }

    submit()

    executions shouldBe 0

  test("an invalid selection displays its error"):
    val runner: ExperimentDialog.RunExperiment = _ => Future.successful(Right("report"))
    open(runner)
    onFxThread {
      allFormComboBoxes.head.setValue("Unknown")
    }

    submit()

    onFxThread(resultOutput.getText) should include("Invalid value 'Unknown'")

  test("a successful experiment displays its report"):
    val (runner, result) = pendingRunner
    open(runner)
    submit()

    result.success(Right("completed report"))
    drainFxQueue()

    onFxThread(resultOutput.getText) shouldBe "completed report"

  test("a failed experiment displays the failure heading"):
    val error            = InvalidPerformanceArgument("argument", "value")
    val (runner, result) = pendingRunner
    open(runner)
    submit()

    result.success(Left(error))
    drainFxQueue()

    onFxThread(resultOutput.getText) should startWith("Performance test failed:\n")

  test("a failed experiment displays its error message"):
    val error            = InvalidPerformanceArgument("argument", "value")
    val (runner, result) = pendingRunner
    open(runner)
    submit()

    result.success(Left(error))
    drainFxQueue()

    onFxThread(resultOutput.getText) should include(error.message)

  test("an exceptional experiment displays the exception message"):
    val runner: ExperimentDialog.RunExperiment =
      _ => Future.failed(new IllegalStateException("unexpected failure"))
    open(runner)

    submit()
    drainFxQueue()

    onFxThread(resultOutput.getText) should include("unexpected failure")

  test("an exception without a message displays its class name"):
    val runner: ExperimentDialog.RunExperiment = _ => Future.failed(new IllegalStateException())
    open(runner)

    submit()
    drainFxQueue()

    onFxThread(resultOutput.getText) should include("IllegalStateException")

  test("a completed experiment reuses the running result dialog"):
    val (runner, result) = pendingRunner
    open(runner)
    submit()
    val runningStage = onFxThread(resultStage.value)

    result.success(Right("completed report"))
    drainFxQueue()

    onFxThread(resultStage.value) should be theSameInstanceAs runningStage

  test("the public result title matches the result dialog"):
    val result = ExperimentDialog.ResultTitle

    result shouldBe ResultDialog.Title

  test("the performance form matches its architectural snapshot"):
    val (runner, _) = pendingRunner
    open(runner)

    val stage = onFxThread(getRequiredActiveStage)

    assertMatchesArchitecturalSnapshotOfStage(
      "performance_test_dialog_initial",
      stage
    )

  test("the performance form matches its visual snapshot"):
    val (runner, _) = pendingRunner
    open(runner)

    val root: scalafx.scene.Node = onFxThread(getRequiredActiveStage.getScene.getRoot)

    assertMatchesVisualSnapshot(
      "performance_test_dialog_initial",
      root,
      maxDiffPercentage = 8.0
    )
