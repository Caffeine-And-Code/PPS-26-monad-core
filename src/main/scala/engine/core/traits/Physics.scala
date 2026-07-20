package engine.core.traits

trait Physics[S]:
  def step(scene: S, deltaTime: Long): S
