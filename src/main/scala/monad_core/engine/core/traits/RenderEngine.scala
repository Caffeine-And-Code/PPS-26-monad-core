package monad_core.engine.core.traits

import monad_core.engine.public_api.Painter

private[engine] trait RenderEngine:
  def render(state: State, alpha: Double)(using painter: Painter): Unit