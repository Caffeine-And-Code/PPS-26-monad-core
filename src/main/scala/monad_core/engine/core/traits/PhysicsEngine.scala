package monad_core.engine.core.traits

private[engine] trait PhysicsEngine :
  def step(scene: State, dt: Long): State
