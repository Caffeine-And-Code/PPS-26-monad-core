package monad_core.performance.simulator

import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.performance.core.{PerformanceRequest, PerformanceRunner}
import monad_core.performance.model.{EnginePerformanceError, NanoClock, PerformanceError, PerformanceReport}

/**
 * Connects the generic performance runner to a real engine tick.
 *
 * @see
 *   [[monad_core.performance.core.PerformanceRunner PerformanceRunner]] and
 *   [[monad_core.engine.simulator.EngineFacade EngineFacade]]
 */
object EnginePerformance:

  /**
   * Runs a performance experiment using the supplied physics rules.
   *
   * @param request
   *   performance strategy and configuration
   * @param physicsManager
   *   physics rules applied during every measured engine tick
   * @param clock
   *   monotonic clock used by the performance runner
   * @return
   *   the completed report, or the first scene or engine error
   * @see
   *   [[monad_core.performance.core.PerformanceRunner PerformanceRunner]]
   */
  def run(
      request: PerformanceRequest,
      physicsManager: PhysicsManager
  )(using clock: NanoClock): Either[PerformanceError, PerformanceReport] =
    PerformanceRunner.run(request, prepareWith(physicsManager))

  /**
   * Creates a workload factory bound to one physics manager.
   *
   * Each prepared workload owns an engine session and a deterministic scene. Executing the
   * returned operation advances that scene by one engine tick.
   *
   * @param physicsManager
   *   physics rules applied by the workload
   * @return
   *   function that prepares a tick operation for an entity count
   * @see
   *   [[monad_core.performance.simulator.DeterministicScene DeterministicScene]] and
   *   [[monad_core.engine.simulator.EngineFacade EngineFacade]]
   */
  private def prepareWith(
      physicsManager: PhysicsManager
  ): PerformanceRunner.PrepareWorkload = entityCount =>
    DeterministicScene(entityCount).map { scene =>
      val session = EngineFacade.start(EngineFacade.default)

      () =>
        EngineFacade
          .tick(
            session = session,
            state = scene,
            currentTime = EngineFacade.DefaultTickTime,
            physics = physicsManager
          )
          .left
          .map(EnginePerformanceError.apply)
          .map(_ => ())
    }
