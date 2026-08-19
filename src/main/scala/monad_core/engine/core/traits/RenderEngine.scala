package monad_core.engine.core.traits

import monad_core.engine.simulator.Painter

private[engine] trait RenderEngine:
  def render(state: State)(using painter: Painter): Unit
