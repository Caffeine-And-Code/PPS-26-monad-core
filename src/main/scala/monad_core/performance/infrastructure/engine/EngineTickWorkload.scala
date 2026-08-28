package monad_core.performance.infrastructure.engine

import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.performance.application.{PerformanceWorkload, SampleCollector}
import monad_core.performance.domain.{EnginePerformanceError, EntityCount, PerformanceError}

/** Performance workload that measures one tick of the simulation engine. */
object EngineTickWorkload extends PerformanceWorkload:

  /**
   * Prepares a repeatable engine-tick operation for the requested scene size, using the default
   * [[monad_core.engine.physics.core.PhysicsManager]] implementation.
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
    prepareWith(entityCount, PhysicsManager.default())

  /**
   * Creates a workload that uses the supplied immutable Physics Manager snapshot.
   *
   * @param physicsManager
   *   immutable snapshot of the Physics Manager to use for the workload
   * @return
   *   workload retaining the supplied manager
   */
  def withPhysicsManager(physicsManager: PhysicsManager): PerformanceWorkload =
    (entityCount: EntityCount) => prepareWith(entityCount, physicsManager)

  /** Prepares one engine tick with the selected physics manager. */
  private def prepareWith(
      entityCount: EntityCount,
      physicsManager: PhysicsManager
  ): Either[PerformanceError, SampleCollector.Operation] =
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
