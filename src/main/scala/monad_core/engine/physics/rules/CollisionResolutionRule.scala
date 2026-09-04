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

private[physics] object CollisionResolutionRule:
  private val Id = "collision-resolution"

  given collisionResolutionRule: PhysicsRule with

    override val RuleId: String = CollisionResolutionRule.Id

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

    private def toEvent(detected: EntityCollisionContact): CollisionDetected =
      CollisionDetected(
        entityId = detected.firstId,
        target = CollisionTarget.Entity(detected.secondId),
        collision = detected.collision
      )
