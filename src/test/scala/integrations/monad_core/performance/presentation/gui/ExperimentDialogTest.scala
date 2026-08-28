package integrations.monad_core.performance.presentation.gui

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import javafx.scene.control.TextArea
import javafx.stage.Window
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.performance.domain.{
  DurationConversion,
  InvalidEntityCount,
  PerformanceConfig,
  PerformanceError
}
import monad_core.performance.helpers.SequenceNanoClock
import monad_core.performance.infrastructure.engine.EnginePerformanceExperiment
import monad_core.performance.presentation.PerformanceArguments
import monad_core.performance.presentation.gui.*
import monad_core.simulator.infrastructure.engine.MonadCoreGameEngineRuntime
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import scalafx.Includes.jfxNode2sfx
import scalafx.scene.control.ComboBox

import scala.concurrent.Promise
import scala.jdk.CollectionConverters.*

class ExperimentDialogTest
    extends AnyFunSuite
    with Matchers
    with Eventually
    with DialogTesting
    with FormTesting:

  private val SingleEntityCount = 1
  private val SingleIteration   = 1
  private val NoWarmups         = 0

  private val SingleExecutionValues = Map(
    PerformanceArguments.Entities   -> SingleEntityCount.toString,
    PerformanceArguments.Iterations -> SingleIteration.toString,
    PerformanceArguments.Warmups    -> NoWarmups.toString,
    PerformanceArguments.FrameBudgetMillis ->
      DurationConversion
        .nanosToWholeMillis(PerformanceConfig.DefaultFrameBudgetNanos)
        .toString
  )

  private val RunnerTimeout         = Span(5, Seconds)
  private val RunnerPollingInterval = Span(10, Millis)

  private val CommonFieldIds = Set(
    PerformanceArguments.Entities,
    PerformanceArguments.Iterations,
    PerformanceArguments.Warmups,
    PerformanceArguments.FrameBudgetMillis
  )

  private def pendingProps: ExperimentDialogProps =
    val pendingResult = Promise[Either[PerformanceError, String]]()
    ExperimentDialogProps(
      physicsManager = () => PhysicsManager.default(),
      runner = (_, _) => pendingResult.future
    )

  private def resultStage: Option[javafx.stage.Stage] =
    Window.getWindows.asScala.collectFirst {
      case stage: javafx.stage.Stage
          if stage.isShowing && stage.getTitle == ExperimentDialog.ResultTitle =>
        stage
    }

  private def select(testType: ExperimentType): Unit =
    val selector = new ComboBox[String](allFormComboBoxes.head)
    selector.selectionModel().select(testType.label)

  private def visibleFieldIds: Set[String] =
    allFormFields.map(_.getId).toSet

  test("the dialog keeps common fields and shows stress-specific fields"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))

      allFormFields.map(field => field.getId -> field.getText).toMap shouldBe Map(
        PerformanceArguments.Entities          -> "100",
        PerformanceArguments.Iterations        -> "20",
        PerformanceArguments.Warmups           -> "5",
        PerformanceArguments.FrameBudgetMillis -> "16",
        PerformanceArguments.MaximumEntities   -> "1600",
        PerformanceArguments.GrowthFactor      -> "2"
      )
    }

  test("the selector offers every supported test type"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))

      allFormComboBoxes.head.getItems.asScala.toSeq shouldBe
        ExperimentType.values.map(_.label).toSeq
    }

  test("the selector starts from stress"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))

      allFormComboBoxes.head.getValue shouldBe ExperimentType.Stress.label
    }

  test("selecting load keeps every common field without additional fields"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))
      select(ExperimentType.Load)

      visibleFieldIds shouldBe CommonFieldIds
    }

  test("selecting spike keeps common fields and adds maximum entities"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))
      select(ExperimentType.Spike)

      visibleFieldIds shouldBe CommonFieldIds + PerformanceArguments.MaximumEntities
    }

  test("selecting scalability keeps common fields and adds its growth fields"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))
      select(ExperimentType.Scalability)

      visibleFieldIds shouldBe CommonFieldIds ++ Set(
        PerformanceArguments.MaximumEntities,
        PerformanceArguments.GrowthFactor
      )
    }

  test("the dialog can use its real asynchronous runner"):
    onFxThread {
      val result = ExperimentDialog.show(() => PhysicsManager.default())

      result shouldBe Right(())
    }

  test("the dialog uses an explicit run action"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))

      formSaveButton.getText shouldBe ExperimentDialog.SubmitLabel
    }

  test("the dialog matches its initial visual snapshot"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))
      val rootNode: scalafx.scene.Node = getRequiredActiveStage.getScene.getRoot

      assertMatchesVisualSnapshot(
        "performance_test_dialog_initial",
        rootNode,
        maxDiffPercentage = 8.0
      )
    }

  test("submitting a performance test immediately displays its running state"):
    onFxThread {
      getOrFail(ExperimentDialog.show(pendingProps))

      formSaveButton.fire()

      val output = resultStage.value.getScene.getRoot
        .lookup(".performance-result-output")
        .asInstanceOf[TextArea]
      output.getText should include("running")
    }

  test("the GUI runs the selected test with current engine rules and displays its result"):
    val runtime      = MonadCoreGameEngineRuntime()
    val disabledRule = runtime.physicsRules.head
    runtime.setPhysicsRuleEnabled(disabledRule.id, isEnabled = false)
    val pendingResult = Promise[Either[PerformanceError, String]]()
    var receivedRequest: Option[ExperimentRequest] = None
    var receivedPhysics: Option[PhysicsManager]               = None
    val runner: ExperimentExecutor = (request, physics) =>
      receivedRequest = Some(request)
      receivedPhysics = Some(physics)
      pendingResult.future
    val props = ExperimentDialogProps(
      physicsManager = () => runtime.physicsManagerSnapshot,
      runner = runner
    )

    onFxThread {
      getOrFail(ExperimentDialog.show(props))
      select(ExperimentType.Load)
      val values = Map(
        PerformanceArguments.Entities          -> "1",
        PerformanceArguments.Iterations        -> "1",
        PerformanceArguments.Warmups           -> "0",
        PerformanceArguments.FrameBudgetMillis -> "16"
      )
      allFormFields.foreach(field => field.setText(values(field.getId)))
      formSaveButton.fire()
    }

    val runningStage    = onFxThread(resultStage.value)
    val request         = receivedRequest.value
    val selectedPhysics = receivedPhysics.value
    val clock           = SequenceNanoClock(Vector(0L, 1L))
    val result = EnginePerformanceExperiment.run(
      request.route,
      request.arguments,
      selectedPhysics
    )(using clock)
    pendingResult.success(result)
    drainFxQueue()

    EngineFacade
      .physicsRules(selectedPhysics)
      .find(_.id == disabledRule.id)
      .map(_.isEnabled) shouldBe Some(false)
    onFxThread {
      resultStage.value should be theSameInstanceAs runningStage
      val output = resultStage.value.getScene.getRoot
        .lookup(".performance-result-output")
        .asInstanceOf[TextArea]
      output.getText should include("Performance experiment: Load")
    }

  test("the performance-test dialog displays a validation error as a result"):
    val error = InvalidEntityCount(0)
    val props = ExperimentDialogProps(
      physicsManager = () => PhysicsManager.default(),
      runner = (_, _) => scala.concurrent.Future.successful(Left(error))
    )

    onFxThread {
      getOrFail(ExperimentDialog.show(props))
      formSaveButton.fire()
    }
    drainFxQueue()

    onFxThread {
      val output = resultStage.value.getScene.getRoot
        .lookup(".performance-result-output")
        .asInstanceOf[TextArea]

      output.getText should include("Performance test failed:")
      output.getText should include(error.message)
    }

  test("submitting the dialog executes the default engine runner"):
    val physicsManager = PhysicsManager.default().disableAll

    val output = onFxThread {
      getOrFail(ExperimentDialog.show(() => physicsManager))
      select(ExperimentType.Load)
      allFormFields.foreach(field => field.setText(SingleExecutionValues(field.getId)))
      formSaveButton.fire()
      resultStage.value.getScene.getRoot
        .lookup(".performance-result-output")
        .asInstanceOf[TextArea]
    }

    eventually(timeout(RunnerTimeout), interval(RunnerPollingInterval)) {
      onFxThread(output.getText) should include(
        s"Performance experiment: ${ExperimentType.Load.label}"
      )
    }
