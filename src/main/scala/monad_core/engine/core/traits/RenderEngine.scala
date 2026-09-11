package monad_core.engine.core.traits

import monad_core.engine.model.EngineError
import monad_core.engine.simulator.{DrawCommand, Painter}

private[engine] trait RenderEngine:
  def render(state: State)(using painter: Painter): Either[EngineError, Vector[DrawCommand]]
