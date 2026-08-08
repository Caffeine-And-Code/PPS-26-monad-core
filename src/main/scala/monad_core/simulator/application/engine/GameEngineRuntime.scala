package monad_core.simulator.application.engine

import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.world.World

trait GameEngineRuntime extends EngineControl:
  def reset(word: World): Unit
  def attach(renderer: World => Unit)(using Painter): Unit
