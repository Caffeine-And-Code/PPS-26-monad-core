package monad_core.engine.public_api

import monad_core.engine.core.{GameLoop, RendererManager}
import monad_core.engine.core.traits.{PhysicsEngine, State}

object EngineFacade:
  def tick(loop: GameLoop, state: State, currentTime: Long)
          (using painter: Painter, physics: PhysicsEngine, renderer: RendererManager.type): (State, GameLoop) =
    loop.tick(state, currentTime).toOption.get