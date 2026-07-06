package engine.core.traits

trait Physics :
  def step[S](scene: S, dt: Long): S
