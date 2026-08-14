package monad_core.engine.public_api

import monad_core.engine.core.{GameLoop, RendererManager}
import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.errors.EngineError

object EngineFacade:

  def tick(loop: GameLoop, state: State, currentTime: Long)(using
      painter: Painter,
      physics: PhysicsEngine
  ): Either[EngineError, (State, GameLoop)] =
    loop.tick(state, currentTime)
