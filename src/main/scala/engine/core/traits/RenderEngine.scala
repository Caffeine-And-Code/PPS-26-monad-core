package engine.core.traits

trait RenderEngine[S]:
  def render(scene: S, alpha: Double): Unit