package monad_core.engine.core.traits

import monad_core.engine.model.EngineError
import monad_core.engine.simulator.Painter

private[engine] trait RenderEngine:
  def render(state: State, alpha: Double)(using painter: Painter): Either[EngineError, Unit]
