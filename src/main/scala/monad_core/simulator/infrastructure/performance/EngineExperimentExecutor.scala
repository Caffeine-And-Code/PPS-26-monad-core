package monad_core.simulator.infrastructure.performance

import monad_core.engine.physics.core.PhysicsManager
import monad_core.performance.application.NanoClock
import monad_core.performance.infrastructure.SystemNanoClock
import monad_core.performance.infrastructure.engine.EnginePerformanceExperiment
import monad_core.simulator.application.performance.ExperimentExecutor

import scala.concurrent.{ExecutionContext, Future}

/** Engine-backed executor used by the graphical performance interface. */
object EngineExperimentExecutor:

  /** Executes the selected engine command outside the graphical thread. */
  given default: ExperimentExecutor[PhysicsManager] = (request, physicsManager) =>
    Future {
      given NanoClock = SystemNanoClock
      EnginePerformanceExperiment.run(request.route, request.arguments, physicsManager)
    }(ExecutionContext.global)
