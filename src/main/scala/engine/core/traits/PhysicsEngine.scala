package engine.core.traits

trait PhysicsEngine :
  def step(scene: Scene, dt: Long): Scene
