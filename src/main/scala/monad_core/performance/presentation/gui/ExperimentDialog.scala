package monad_core.performance.presentation.gui

import monad_core.engine.physics.core.PhysicsManager
import monad_core.performance.application.NanoClock
import monad_core.performance.domain.{DurationConversion, PerformanceConfig}
import monad_core.performance.infrastructure.SystemNanoClock
import monad_core.performance.infrastructure.engine.EnginePerformanceExperiment
import monad_core.performance.presentation.PerformanceArguments
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.{Error, NotificationManager}
import monad_core.simulator.presentation.components.forms.base.{
  FormDialog,
  FormDialogProps,
  FormFieldSpec,
  SelectFieldSpec,
  TextFieldSpec
}
import scalafx.application.Platform
import scalafx.stage.Window

import scala.concurrent.{ExecutionContext, Future}

/** Dependencies required to launch a graphical performance test. */
final case class ExperimentDialogProps(
                                        physicsManager: () => PhysicsManager,
                                        runner: ExperimentExecutor,
                                        owner: Option[Window] = None
)

/** Dynamic parameter form and result orchestration for all performance experiments. */
object ExperimentDialog:

  /** Input-window title. */
  val Title = "Performance Test"

  /** Result-window title exposed for presentation integration. */
  val ResultTitle: String = ResultDialog.Title

  /** Explicit label used to start the experiment. */
  val SubmitLabel = "Run"

  /** Preferred width of the parameter form. */
  private val FormWidth = 520.0

  /** Text shown before a failed performance result. */
  private val FailureHeader = "Performance test failed:"

  /** Text displayed while the selected experiment is running. */
  private val RunningMessage = "Performance test running..."
  
  private val StartEntitiesLabel = "Start entities"
  private val MaximumEntitiesLabel = "Maximum entities"
  private val GrowthFactorLabel = "Growth factor"
  private val IterationsLabel = "Iterations"
  private val WarmupsLabel = "Warm-ups"
  private val FrameBudgetLabel = "Frame budget (ms)"

  /** Editable fields indexed by the command argument they produce. */
  private val ArgumentFields: Map[String, TextFieldSpec] = Map(
    PerformanceArguments.Entities -> TextFieldSpec(
      PerformanceArguments.Entities,
      StartEntitiesLabel,
      Some(PerformanceConfig.DefaultStartEntities.toString)
    ),
    PerformanceArguments.MaximumEntities -> TextFieldSpec(
      PerformanceArguments.MaximumEntities,
      MaximumEntitiesLabel,
      Some(PerformanceConfig.DefaultMaximumEntities.toString)
    ),
    PerformanceArguments.GrowthFactor -> TextFieldSpec(
      PerformanceArguments.GrowthFactor,
      GrowthFactorLabel,
      Some(PerformanceConfig.DefaultGrowthFactor.toString)
    ),
    PerformanceArguments.Iterations -> TextFieldSpec(
      PerformanceArguments.Iterations,
      IterationsLabel,
      Some(PerformanceConfig.DefaultIterations.toString)
    ),
    PerformanceArguments.Warmups -> TextFieldSpec(
      PerformanceArguments.Warmups,
      WarmupsLabel,
      Some(PerformanceConfig.DefaultWarmups.toString)
    ),
    PerformanceArguments.FrameBudgetMillis -> TextFieldSpec(
      PerformanceArguments.FrameBudgetMillis,
      FrameBudgetLabel,
      Some(
        DurationConversion
          .nanosToWholeMillis(PerformanceConfig.DefaultFrameBudgetNanos)
          .toString
      )
    )
  )

  /** Arguments represented by fields that are always visible and editable. */
  private val CommonArguments = Vector(
    PerformanceArguments.Entities,
    PerformanceArguments.Iterations,
    PerformanceArguments.Warmups,
    PerformanceArguments.FrameBudgetMillis
  )

  /** Complete dynamic form: selector, common fields, then selected specific fields. */
  private val Fields: Seq[FormFieldSpec] =
    SelectFieldSpec(
      id = ExperimentFormArguments.PerformanceExperimentType,
      label = "Test type",
      options = ExperimentType.values.map(_.label).toSeq,
      defaultValue = Some(ExperimentType.Default.label),
      dependentFields = ExperimentType.values
        .map(experimentType => experimentType.label -> fieldsFor(experimentType.specificArguments))
        .toMap
    ) +: fieldsFor(CommonArguments)

  /**
   * Opens the form using the real engine workload and system clock.
   *
   * @param physicsManager
   *   provider read only when the user submits the form
   * @return
   *   form construction result
   */
  def show(physicsManager: () => PhysicsManager): Either[BaseError, Unit] =
    show(
      ExperimentDialogProps(
        physicsManager = physicsManager,
        runner = defaultRunner
      )
    )

  /** Opens the performance form with explicit, testable dependencies. */
  def show(props: ExperimentDialogProps): Either[BaseError, Unit] =
    val viewModel = ExperimentViewModel(
      props.runner,
      props.physicsManager,
      action => Platform.runLater(action())
    )

    var resultDialog = Option.empty[ResultDialogHandle]
    viewModel.state.onChange { (_, _, state) =>
      resultDialog = displayState(state, props.owner, resultDialog)
    }

    FormDialog.show(
      FormDialogProps(
        title = Title,
        fields = Fields,
        onSubmit = viewModel.onSubmit,
        owner = props.owner,
        minWidth = FormWidth,
        submitLabel = SubmitLabel
      )
    )

  /** Converts known argument identifiers to their graphical fields. */
  private def fieldsFor(arguments: Vector[String]): Seq[FormFieldSpec] =
    arguments.flatMap(ArgumentFields.get)

  /** Executes the selected engine command outside the graphical thread. */
  private[gui] val defaultRunner: ExperimentExecutor = (request, physicsManager) =>
    Future {
      given NanoClock = SystemNanoClock
      EnginePerformanceExperiment.run(request.route, request.arguments, physicsManager)
    }(ExecutionContext.global)

  /** Displays the current execution state in one reusable result window. */
  private[gui] def displayState(
                                 state: ExperimentState,
                                 owner: Option[Window],
                                 currentDialog: Option[ResultDialogHandle]
  ): Option[ResultDialogHandle] =
    state match
      case ExperimentState.Succeeded(report) =>
        displayResult(report, owner, currentDialog)
      case ExperimentState.Failed(message) =>
        displayResult(s"$FailureHeader\n$message", owner, currentDialog)
      case ExperimentState.Running =>
        displayResult(RunningMessage, owner, currentDialog)
      case ExperimentState.Ready => currentDialog

  /** Opens or updates the result and reports an unexpected graphical failure. */
  private def displayResult(
      content: String,
      owner: Option[Window],
      currentDialog: Option[ResultDialogHandle]
  ): Option[ResultDialogHandle] =
    currentDialog match
      case Some(dialog) =>
        dialog.update(content)
        currentDialog
      case None =>
        ResultDialog.open(content, owner) match
          case Right(dialog) => Some(dialog)
          case Left(error) =>
            NotificationManager.show(error.message, Error)
            None
