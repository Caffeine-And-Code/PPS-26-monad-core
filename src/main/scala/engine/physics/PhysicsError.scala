package engine.physics

import engine.errors.EngineError

sealed abstract class PhysicsError(message: String) extends EngineError(message)

case class NegativeDeltaTime(dt: Long)
  extends PhysicsError(s"Delta time cannot be negative: $dt")