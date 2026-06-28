package engine.physics

import engine.core.traits.{Physics, Scene}

case class PhysicsEngine() extends Physics {
  override def step(scene: Scene, dt: Long): Scene = scene
}
