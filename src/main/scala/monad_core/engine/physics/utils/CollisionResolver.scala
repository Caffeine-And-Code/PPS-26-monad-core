package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.PhysicsError
import monad_core.engine.physics.utils.CollisionMap

/** Resolves overlap and bounce responses for all colliding mobile entities. */
private[physics] object CollisionResolver:

  /**
   * Resolves the collected collisions of every non-fixed entity.
   *
   * @param collisions
   *   collisions grouped by affected entity
   * @return
   *   updated mobile entities, or the first physics error
   */
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

  /**
   * Resolves, in order, overlaps and bounce responses to one entity.
   *
   * @param entity
   *   affected entity
   * @param collisions
   *   entities and contacts colliding with it
   * @return
   *   fully corrected entity, or the first [[PhysicsError]]
   */
  private def resolveCollisions(
      entity: Entity,
      collisions: List[(Entity, Collision)]
  ): Either[PhysicsError, Entity] =
    for
      deOverlappedEntity <- resolveMultipleOverlaps(entity, collisions)
      resolvedEntity     <- resolveMultipleBounces(deOverlappedEntity, collisions)
    yield resolvedEntity

  /**
   * Applies all positional corrections accumulated for one entity.
   *
   * @param entity
   *   entity to de-overlap
   * @param collisions
   *   entities and contacts overlapping it
   * @return
   *   position-corrected entity, or the first [[PhysicsError]]
   */
  private def resolveMultipleOverlaps(
      entity: Entity,
      collisions: List[(Entity, Collision)]
  ): Either[PhysicsError, Entity] =
    collisions.foldLeft(Right(entity): Either[PhysicsError, Entity]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntity), (otherEntity, collision)) =>
        resolveOverlap(updatedEntity, otherEntity, collision)
    }

  /**
   * Corrects one overlap according to the mobility and masses of both bodies.
   *
   * @param entity
   *   entity to move
   * @param other
   *   overlapping entity
   * @param collision
   *   separation normal and penetration depth
   * @return
   *   position-corrected entity, or a [[PhysicsError]]
   */
  private def resolveOverlap(
      entity: Entity,
      other: Entity,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    val newPosition = entity.speed match
      case None =>
        Right(entity.position)
      case _ =>
        other.speed match
          case None =>
            Right(
              PhysicsUtil.pushMobileOverlappingFixed(
                entity.position,
                collision.normalVector,
                collision.penetrationDepth
              )
            )
          case _ =>
            PhysicsUtil.pushMobileOverlappingMobile(
              entity.position,
              collision.normalVector,
              collision.penetrationDepth,
              entity.weight,
              other.weight
            )

    newPosition.map(entity.moveTo)

  /**
   * Applies all velocity responses accumulated for one entity.
   *
   * @param entity
   *   entity to update
   * @param collisions
   *   entities and contacts producing bounce responses
   * @return
   *   velocity-corrected entity, or the first [[PhysicsError]]
   */
  private def resolveMultipleBounces(
      entity: Entity,
      collisions: List[(Entity, Collision)]
  ): Either[PhysicsError, Entity] =
    collisions.foldLeft(Right(entity): Either[PhysicsError, Entity]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntity), (otherEntity, collision)) =>
        BounceResponse(updatedEntity, otherEntity, collision)
    }
