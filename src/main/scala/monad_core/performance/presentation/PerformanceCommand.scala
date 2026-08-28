package monad_core.performance.presentation

import monad_core.performance.application.{NanoClock, PerformanceWorkload}
import monad_core.performance.domain.*

/** Shared command boundary used by command-line and graphical performance clients. */
object PerformanceCommand:

  /**
   * Routes, parses, and executes one performance command.
   *
   * @param route
   *   command name declared by [[PerformanceRoutes]]
   * @param arguments
   *   command-line-shaped option tokens
   * @param workload
   *   workload measured by the selected experiment
   * @param clock
   *   clock used for latency sampling
   * @return
   *   completed experiment report, or the first routing, parsing, or execution error
   */
  def run(route: String, arguments: Array[String])(using
      workload: PerformanceWorkload,
      clock: NanoClock
  ): Either[PerformanceError, ExperimentReport] =
    for
      kind   <- experimentKind(route)
      config <- PerformanceArguments.parse(arguments)
      report <- PerformanceRuntime.run(kind, config)
    yield report

  /** Resolves a public command name to its experiment strategy. */
  private def experimentKind(route: String): Either[PerformanceError, ExperimentKind] =
    route match
      case PerformanceRoutes.Load        => Right(ExperimentKind.Load)
      case PerformanceRoutes.Stress      => Right(ExperimentKind.Stress)
      case PerformanceRoutes.Spike       => Right(ExperimentKind.Spike)
      case PerformanceRoutes.Scalability => Right(ExperimentKind.Scalability)
      case unknown                       => Left(UnknownPerformanceRoute(unknown))
