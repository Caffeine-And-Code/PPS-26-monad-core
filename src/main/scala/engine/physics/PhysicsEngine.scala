package engine.physics

import engine.core.Scene

case class PhysicsEngine()

object PhysicsEngine :

  extension (physicsEngine: PhysicsEngine)
    def step(scene: Scene, dt: Long): Scene =
      require(dt >= 0, "Time difference cannot be negative")

      scene