package monad_core.engine.physics.core

import monad_core.engine.model.EngineError

/** Base type for failures produced while applying physics calculations and rules. */
sealed abstract class PhysicsError(message: String) extends EngineError(message)

/** Indicates that a physics update received a negative nanosecond duration. */
case class NegativeDeltaTime(dt: Long) extends PhysicsError(s"Delta time cannot be negative: $dt")

/** Adapts a domain-model failure produced during a physics update. */
case class PhysicsDomainError(cause: EngineError)
    extends PhysicsError(s"Physics update rejected by the domain model: $cause")

/** Indicates that a physics rule failed for the supplied reason. */
case class PhysicsRuleError(cause: String) extends PhysicsError(s"Physics rule failed: $cause")

/** Indicates that a calculation requiring mass received an entity without weight. */
case class ZeroMassError() extends PhysicsError(s"Mass cannot be zero")

/** Indicates that a ray intersected an identifier absent from the current entity collection. */
case class RayIntersectedAMissingEntity(entityId: String)
    extends PhysicsError(s"Intersected entity is missing: $entityId")

/** Indicates that a ray did not intersect either its target or an obstacle. */
case class RayIntersectedNothing(startId: String, targetId: String)
    extends PhysicsError(s"RayCast intersected nothing: $startId -> $targetId")
