package monad_core.simulator.presentation.performance

import monad_core.performance.presentation.PerformanceArguments
import monad_core.simulator.application.performance.ExperimentRequest
import monad_core.simulator.domain.performance.MissingPerformanceArgument
import monad_core.simulator.errors.BaseError

/** Converts graphical values into a selected performance command. */
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
  def from(values: Map[String, String]): Either[BaseError, ExperimentRequest] =
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
