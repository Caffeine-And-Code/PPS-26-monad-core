package monad_core.performance.infrastructure.engine

import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.performance.application.{PerformanceWorkload, SampleCollector}
import monad_core.performance.domain.{EnginePerformanceError, EntityCount, PerformanceError}

/**
 * Performance workload that measures one tick of the simulation engine.
 *
 * Preparation builds a deterministic scene and initializes one engine session and physics manager.
 * The returned operation reuses these dependencies so setup time is excluded from every sample.
 */
object EngineTickWorkload extends PerformanceWorkload:

  /**
   * Prepares a repeatable engine-tick operation for the requested scene size.
   *
   * Engine failures from scene construction and ticking are translated to
   * [[monad_core.performance.domain.EnginePerformanceError]].
   *
   * @param entityCount
   *   number of entities placed in the deterministic scene
   * @return
   *   an operation that advances the prepared scene by one default-duration tick, or a performance
   *   error if scene preparation fails
   */
  override def prepare(
      entityCount: EntityCount
  ): Either[PerformanceError, SampleCollector.Operation] =
    DeterministicScene(entityCount).map { scene =>
      val session = EngineFacade.start(EngineFacade.default)
      val physics = PhysicsManager.default()

      () =>
        EngineFacade
          .tick(
            session = session,
            state = scene,
            currentTime = EngineFacade.DefaultTickTime,
            physics = physics
          )
          .left
          .map(EnginePerformanceError.apply)
          .map(_ => ())
    }
