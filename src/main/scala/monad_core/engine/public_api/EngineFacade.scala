package monad_core.engine.public_api

import monad_core.engine.core.{GameLoop, PhysicsMock, RendererManager}
import monad_core.engine.core.traits.State

object EngineFacade:
  def tick(loop: GameLoop, state: State, currentTime: Long)
          (using painter: Painter): (State, GameLoop) =
    loop.tick(state, PhysicsMock, RendererManager, currentTime)
