package monad_core.performance.infrastructure.engine

import monad_core.engine.physics.core.PhysicsManager
import monad_core.performance.application.{NanoClock, PerformanceWorkload}
import monad_core.performance.domain.PerformanceError
import monad_core.performance.presentation.{PerformanceCommand, PerformanceReportFormatter}

/** Engine adapter that runs a selected performance command with one physics-manager snapshot. */
object EnginePerformanceExperiment:

  /**
   * Runs and formats one engine performance experiment.
   *
   * @param route
   *   selected performance command
   * @param arguments
   *   command-line-shaped performance options
   * @param physicsManager
   *   immutable physics pipeline selected by the simulator
   * @param clock
   *   monotonic clock used for latency sampling
   * @return
   *   formatted report, or the first command or engine error
   */
  def run(
      route: String,
      arguments: Vector[String],
      physicsManager: PhysicsManager
  )(using clock: NanoClock): Either[PerformanceError, String] =
    given PerformanceWorkload = EngineTickWorkload.withPhysicsManager(physicsManager)

    PerformanceCommand
      .run(route, arguments.toArray)
      .map(PerformanceReportFormatter.format)
