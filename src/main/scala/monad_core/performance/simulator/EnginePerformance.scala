package monad_core.performance.simulator

import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.performance.core.{PerformanceRequest, PerformanceRunner}
import monad_core.performance.model.{EnginePerformanceError, NanoClock, PerformanceError, PerformanceReport}

/** Connects the generic performance runner to a real engine tick. */
object EnginePerformance:

  def run(
      request: PerformanceRequest,
      physicsManager: PhysicsManager
  )(using clock: NanoClock): Either[PerformanceError, PerformanceReport] =
    PerformanceRunner.run(request, prepareWith(physicsManager))

  def prepareWith(
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
