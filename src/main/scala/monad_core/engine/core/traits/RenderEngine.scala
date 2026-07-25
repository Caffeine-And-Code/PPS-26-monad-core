package monad_core.engine.core.traits

trait RenderEngine[S]:
  def render(scene: S, alpha: Double): Unit