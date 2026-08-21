package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.PhysicsError
import monad_core.engine.physics.utils.CollisionMap

object CollisionResolver:

  def apply(
      collisions: CollisionMap
  ): Either[PhysicsError, List[Entity]] =
    collisions
      .filterNot(_._1.isFixed)
      .foldLeft(Right(List.empty): Either[PhysicsError, List[Entity]]) {
        case (Left(err), _) => Left(err)
        case (Right(updatedEntities), (entity, entityCollisions)) =>
          resolveCollisions(entity, entityCollisions).map(updatedEntity =>
            updatedEntities :+ updatedEntity
          )
      }

  private def resolveCollisions(
      entity: Entity,
      collisions: List[(Entity, Collision)]
  ): Either[PhysicsError, Entity] =
    for
      deOverlappedEntity <- resolveMultipleOverlaps(entity, collisions)
      resolvedEntity     <- resolveMultipleBounces(deOverlappedEntity, collisions)
    yield resolvedEntity

  private def resolveMultipleOverlaps(
      entity: Entity,
      collisions: List[(Entity, Collision)]
  ): Either[PhysicsError, Entity] =
    collisions.foldLeft(Right(entity): Either[PhysicsError, Entity]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntity), (otherEntity, collision)) =>
        resolveOverlap(updatedEntity, otherEntity, collision)
    }

  private def resolveOverlap(
      entity: Entity,
      other: Entity,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    val newPosition = entity.speed match
      case None =>
        Right(entity.position)
      case Some(_) =>
        other.speed match
          case None =>
            Right(
              PhysicsUtil.pushMobileOverlappingFixed(
                entity.position,
                collision.normalVector,
                collision.penetrationDepth
              )
            )
          case Some(_) =>
            PhysicsUtil.pushMobileOverlappingMobile(
              entity.position,
              collision.normalVector,
              collision.penetrationDepth,
              entity.weight,
              other.weight
            )

    newPosition.map(entity.moveTo)

  private def resolveMultipleBounces(
      entity: Entity,
      collisions: List[(Entity, Collision)]
  ): Either[PhysicsError, Entity] =
    collisions.foldLeft(Right(entity): Either[PhysicsError, Entity]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntity), (otherEntity, collision)) =>
        BounceResponse(updatedEntity, otherEntity, collision)
    }
