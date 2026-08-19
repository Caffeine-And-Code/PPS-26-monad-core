package monad_core.engine.physics.core

import monad_core.engine.model.{EngineError, Vector2D}

sealed abstract class PhysicsError(message: String) extends EngineError(message)

case class NegativeDeltaTime(dt: Long) extends PhysicsError(s"Delta time cannot be negative: $dt")

case class OutOfBoundEntity(position: Vector2D)
    extends PhysicsError(s"Entity position is out of bounds: (${position.x}, ${position.y})")

case class PhysicsDomainError(cause: EngineError)
    extends PhysicsError(s"Physics update rejected by the domain model: $cause")

case class PhysicsRuleError(cause: String) extends PhysicsError(s"Physics rule failed: $cause")

case class ZeroMassError() extends PhysicsError(s"Mass cannot be zero")

case class RayIntersectedAMissingEntity(entityId: String)
    extends PhysicsError(s"Intersected entity is missing: $entityId")

case class RayIntersectedNothing(startId: String, targetId: String)
    extends PhysicsError(s"RayCast intersected nothing: $startId -> $targetId")
