package monad_core.engine.simulator

import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.core.{GameLoop, GameLoopTickResult}
import monad_core.engine.model.EngineError

object EngineFacade:

  def tick(loop: GameLoop, state: State, currentTime: Long)(using
      physics: PhysicsEngine
  ): Either[EngineError, GameLoopTickResult] =
    loop.tick(state, currentTime)
