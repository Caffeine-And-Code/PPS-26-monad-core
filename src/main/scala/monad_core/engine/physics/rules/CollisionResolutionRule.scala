package monad_core.engine.physics.rules

import monad_core.engine.core.events.EngineEvent.CollisionDetected
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.model.*
import monad_core.engine.physics.core.{
  EntityCollisionContact,
  PhysicsContext,
  PhysicsError,
  PhysicsRule,
  PhysicsRuleResult
}
import monad_core.engine.physics.utils.{
  CollisionMap,
  CollisionResolver,
  PhysicsUtil,
  SceneEntitiesUpdate
}

/** Physics rule that resolves contacts between scene entities. */
private[physics] object CollisionResolutionRule:
  /** Stable identifier of the collision-resolution rule. */
  private val Id = "collision-resolution"

  /** Default entity-collision resolution rule. */
  given collisionResolutionRule: PhysicsRule with

    override val RuleId: String = CollisionResolutionRule.Id

    /**
      * Resolves the entity contacts.
      *
      * @param context
      *   physics context containing the state, elapsed time and entity contacts
      * @return
      *   updated state and entity-collision events, or a [[PhysicsError]]
      */
    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(context.dt)
        activeCollisions = toCollisionMap(context)

        updatedEntities <- CollisionResolver(activeCollisions)

        updatedScene <- SceneEntitiesUpdate(context.state, updatedEntities)
      yield PhysicsRuleResult(
        state = updatedScene,
        events = context.collisions.entityContacts.map(toEvent)
      )

    /**
      * Builds the bidirectional collision map consumed by the resolver.
      * The normal is reversed for the second entity so each response points away from its collider.
      *
      * @param context
      *   physics context containing detected entity contacts
      * @return
      *   collisions grouped by entity
      */
    private def toCollisionMap(context: PhysicsContext): CollisionMap =
      val entitiesById = context.state.allEntities.map(entity => entity.id -> entity).toMap

      context.collisions.entityContacts
        .flatMap { detected =>
          for
            first  <- entitiesById.get(detected.firstId).toVector
            second <- entitiesById.get(detected.secondId).toVector
            entry <- Vector(
              second -> (first, detected.collision),
              first -> (
                second,
                detected.collision.copy(normalVector = detected.collision.normalVector.flip)
              )
            )
          yield entry
        }
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toList)
        .toMap

    /**
      * Converts a detected entity contact into an engine event.
      *
      * @param detected
      *   entity contact to convert
      * @return
      *   corresponding collision event
      */
    private def toEvent(detected: EntityCollisionContact): CollisionDetected =
      CollisionDetected(
        entityId = detected.firstId,
        target = CollisionTarget.Entity(detected.secondId),
        collision = detected.collision
      )
