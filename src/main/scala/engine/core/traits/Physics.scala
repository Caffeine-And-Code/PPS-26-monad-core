package engine.core.traits

trait Physics :
  def step(scene: Scene, dt: Long): Scene
