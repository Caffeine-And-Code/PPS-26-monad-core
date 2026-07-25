package engine.physics.core

import engine.errors.EngineError
import engine.model.Vector2D

sealed abstract class PhysicsError(message: String) extends EngineError(message)

case class NegativeDeltaTime(dt: Long)
  extends PhysicsError(s"Delta time cannot be negative: $dt")

case class OutOfBoundEntity(position: Vector2D)
  extends PhysicsError(s"Entity position is out of bounds: (${position.x}, ${position.y})")