package engine.core.traits

trait PhysicsEngine :
  def step(state: State, dt: Long): State
