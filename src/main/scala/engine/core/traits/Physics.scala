package engine.core.traits

import engine.physics.PhysicsError

trait Physics[S]:
  def step(scene: S, deltaTime: Long): Either[PhysicsError, S]
