package engine.physics

import engine.core.Scene
import engine.model.*

case class PhysicsEngine()

object PhysicsEngine :

  private val NanoInSeconds = 1_000_000_000.0

  extension (physicsEngine: PhysicsEngine)
    def step(scene: Scene, dt: Long): Scene =
      require(dt >= 0, "Time difference cannot be negative")

      scene