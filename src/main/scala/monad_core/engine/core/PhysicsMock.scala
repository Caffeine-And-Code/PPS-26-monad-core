package monad_core.engine.core

import monad_core.engine.core.traits.PhysicsEngine

//TODO: Remove this 
object PhysicsMock extends PhysicsEngine:
  override def step[S](scene: S, dt: Long): S = scene