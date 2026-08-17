package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.physics.core.{PhysicsError, PhysicsRuleError}
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
    resolveMultipleCollisions(entity, collisions)

  private def resolveMultipleCollisions(
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
    val newPosition =
      if other.isFixed then
        Right(
          PhysicsUtil.pushMobileOverlappingFixed(
            entity.position,
            collision.normalVector,
            collision.penetrationDepth
          )
        )
      else
        PhysicsUtil.pushMobileOverlappingMobile(
          entity.position,
          collision.normalVector,
          collision.penetrationDepth,
          entity.weight,
          other.weight
        )

    newPosition match
      case Left(err) => Left(err)
      case Right(p)  => Right(entity.moveTo(p))

  private def resolveMultipleBounces(
      entity: Entity,
      collisions: List[(Entity, Collision)]
  ): Either[PhysicsError, Entity] =
    for
      currentSpeed <- entity.speed match
        case Some(s) => Right(s)
        case None    => Left(PhysicsRuleError("Entity has no speed to resolve bounce"))

      updatedEntity <- collisions
        .foldLeft(Right(currentSpeed): Either[PhysicsError, Vector2D]) {
          case (Left(err), _) => Left(err)
          case (Right(updatedSpeed), (otherEntity, collision)) =>
            resolveBounce(entity, otherEntity, updatedSpeed, collision)
        }
        .flatMap { finalSpeed =>
          Right(entity.withSpeed(finalSpeed))
        }
    yield updatedEntity

  private def resolveBounce(
      entity: Entity,
      other: Entity,
      entitySpeed: Vector2D,
      collision: Collision
  ): Either[PhysicsError, Vector2D] =

    other.speed match
      case None => Right(
        PhysicsUtil.reflectOnFixed(
          entitySpeed,
          collision.normalVector
        )
      )
      case Some(s) => PhysicsUtil.reflectOnMobile(
        entitySpeed,
        s,
        collision.normalVector,
        entity.weight,
        other.weight
      )
