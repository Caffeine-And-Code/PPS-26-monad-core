package monad_core.engine.core.traits

private[engine] trait PhysicsEngine :
  def step[S](scene: S, dt: Long): S
