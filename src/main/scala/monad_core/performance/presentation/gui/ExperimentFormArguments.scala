package monad_core.performance.presentation.gui

import monad_core.performance.domain.{MissingPerformanceArgument, PerformanceError}
import monad_core.performance.presentation.PerformanceArguments

/** Immutable command selected and populated by the graphical performance form. */
final case class ExperimentRequest(route: String, arguments: Vector[String])

/** Converts graphical values into one selected performance command. */
object ExperimentFormArguments:

  /** Form key containing the selected performance-test type. */
  val PerformanceExperimentType = "performance-experiment-type"

  /** Arguments editable and used by every experiment. */
  private val CommonArguments = Vector(
    PerformanceArguments.Entities,
    PerformanceArguments.Iterations,
    PerformanceArguments.Warmups,
    PerformanceArguments.FrameBudgetMillis
  )

  /**
   * Creates a request containing all common arguments and only the selected specific arguments.
   *
   * @param values
   *   submitted graphical field values
   * @return
   *   selected route and ordered command tokens, or a missing or unsupported selection error
   */
  def from(values: Map[String, String]): Either[PerformanceError, ExperimentRequest] =
    for
      label <- values
        .get(PerformanceExperimentType)
        .toRight(MissingPerformanceArgument(PerformanceExperimentType))
      experimentType <- ExperimentType.fromLabel(label)
    yield ExperimentRequest(
      route = experimentType.route,
      arguments = argumentsFor(CommonArguments ++ experimentType.specificArguments, values)
    )

  /** Converts present fields to stable `--name value` argument pairs. */
  private def argumentsFor(
      arguments: Vector[String],
      values: Map[String, String]
  ): Vector[String] =
    arguments.flatMap(argument =>
      values.get(argument).toVector.flatMap(value => Vector(argument, value))
    )
