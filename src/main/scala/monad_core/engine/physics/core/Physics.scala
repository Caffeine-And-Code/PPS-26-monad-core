package monad_core.engine.physics.core

trait Physics[S]:
  def step(scene: S, deltaTime: Long): Either[PhysicsError, S]
