package monad_core.engine.core.traits

trait PhysicsEngine :
  def step[S](scene: S, dt: Long): S
