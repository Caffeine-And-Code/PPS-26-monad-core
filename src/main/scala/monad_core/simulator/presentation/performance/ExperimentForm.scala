package monad_core.simulator.presentation.performance

import monad_core.simulator.domain.performance.{InvalidPerformanceArgument, PerformanceError}
import monad_core.simulator.application.performance.PerformanceExecutor
import monad_core.simulator.presentation.components.forms.base.{
  FormFieldSpec,
  SelectFieldSpec,
  TextFieldSpec
}

/**
 * Command selected and populated by the graphical performance form.
 *
 * @param route
 *   performance route to execute
 * @param arguments
 *   command-line option and value pairs collected by the form
 */
final case class ExperimentCommand(route: String, arguments: Vector[String])

/**
 * Defines the performance form and converts its values into a command.
 *
 * @see [[PerformanceExecutor PerformanceExecutor]]
 */
object ExperimentForm:

  /**
   * Test type selectable from the form.
   *
   * @param label
   *   value shown by the selector
   * @param route
   *   command-line route associated with the choice
   * @param specificFields
   *   arguments displayed only for this choice
   */
  private enum ExperimentChoice(
      val label: String,
      val route: String,
      val specificFields: Vector[String]
  ):

    case Load
        extends ExperimentChoice(
          "Load",
          PerformanceExecutor.LoadRoute,
          Vector.empty
        )

    case Stress
        extends ExperimentChoice(
          "Stress",
          PerformanceExecutor.StressRoute,
          Vector(PerformanceExecutor.MaximumEntities, PerformanceExecutor.GrowthFactor)
        )

    case Spike
        extends ExperimentChoice(
          "Spike",
          PerformanceExecutor.SpikeRoute,
          Vector(PerformanceExecutor.MaximumEntities)
        )

    case Scalability
        extends ExperimentChoice(
          "Scalability",
          PerformanceExecutor.ScalabilityRoute,
          Vector(PerformanceExecutor.MaximumEntities, PerformanceExecutor.GrowthFactor)
        )

  private val KindField = "performance-kind"

  private val CommonFields = Vector(
    PerformanceExecutor.Entities,
    PerformanceExecutor.Iterations,
    PerformanceExecutor.Warmups,
    PerformanceExecutor.FrameBudgetMillis
  )

  private val ArgumentFields: Map[String, TextFieldSpec] = Map(
    PerformanceExecutor.Entities -> field(
      PerformanceExecutor.Entities,
      "Start entities",
      PerformanceExecutor.DefaultStartEntities
    ),
    PerformanceExecutor.MaximumEntities -> field(
      PerformanceExecutor.MaximumEntities,
      "Maximum entities",
      PerformanceExecutor.DefaultMaximumEntities
    ),
    PerformanceExecutor.GrowthFactor -> field(
      PerformanceExecutor.GrowthFactor,
      "Growth factor",
      PerformanceExecutor.DefaultGrowthFactor
    ),
    PerformanceExecutor.Iterations -> field(
      PerformanceExecutor.Iterations,
      "Iterations",
      PerformanceExecutor.DefaultIterations
    ),
    PerformanceExecutor.Warmups -> field(
      PerformanceExecutor.Warmups,
      "Warm-ups",
      PerformanceExecutor.DefaultWarmups
    ),
    PerformanceExecutor.FrameBudgetMillis -> TextFieldSpec(
      PerformanceExecutor.FrameBudgetMillis,
      "Frame budget (ms)",
      Some(PerformanceExecutor.DefaultFrameBudgetMillis.toString)
    )
  )

  /** Fields displayed by the graphical performance dialog. */
  val fields: Seq[FormFieldSpec] =
    SelectFieldSpec(
      id = KindField,
      label = "Test type",
      options = ExperimentChoice.values.map(_.label).toSeq,
      defaultValue = Some(ExperimentChoice.Stress.label),
      dependentFields = ExperimentChoice.values
        .map(choice => choice.label -> fieldsFor(choice.specificFields))
        .toMap
    ) +: fieldsFor(CommonFields)

  /**
   * Converts submitted graphical values into a performance command.
   *
   * @param values values indexed by their form-field identifiers
   * @return the selected route and its arguments, or an invalid selection error
   */
  def command(values: Map[String, String]): Either[PerformanceError, ExperimentCommand] =
    for choice <- experimentChoice(values)
    yield ExperimentCommand(
      choice.route,
      argumentsFor(CommonFields ++ choice.specificFields, values)
    )

  /**
   * Creates a textual integer field with its default value.
   *
   * @param id
   *  field identifier
   * @param label
   *  field label
   * @param default
   *  initial integer value
   * @return
   *  textual field declaration
   */
  private def field(id: String, label: String, default: Int): TextFieldSpec =
    TextFieldSpec(id, label, Some(default.toString))

  /**
   * Resolves argument identifiers to their form-field declarations.
   *
   * @param arguments
   *  argument identifiers to resolve
   * @return
   *  known field declarations in the supplied order
   */
  private def fieldsFor(arguments: Vector[String]): Seq[FormFieldSpec] =
    arguments.flatMap(ArgumentFields.get)

  /**
   * Resolves the selected test type.
   *
   * @param values
   *  submitted values indexed by field identifier
   * @return
   *  the selected choice, or an invalid-argument error
   */
  private def experimentChoice(
      values: Map[String, String]
  ): Either[PerformanceError, ExperimentChoice] =
    val value = values.getOrElse(KindField, "")
    ExperimentChoice.values
      .find(_.label == value)
      .toRight(InvalidPerformanceArgument(KindField, value))

  /**
   * Converts the available values into ordered option and value pairs.
   *
   * Missing values are omitted so that command-line defaults remain applicable.
   *
   * @param arguments
   *  ordered argument identifiers
   * @param values
   *  submitted values indexed by field identifier
   * @return
   *  flattened command-line arguments
   */
  private def argumentsFor(
      arguments: Vector[String],
      values: Map[String, String]
  ): Vector[String] =
    arguments.flatMap(argument =>
      values.get(argument).toVector.flatMap(value => Vector(argument, value))
    )
