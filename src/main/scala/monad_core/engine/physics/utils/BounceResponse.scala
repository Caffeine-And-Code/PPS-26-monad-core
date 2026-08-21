package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.PhysicsError

private[physics] object BounceResponse:

  def apply(
      entity: Entity,
      other: Entity,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    (entity.speed, entity.angularSpeed) match
      case (Some(speed), None) =>
        resolveSimpleBounce(entity, other, speed, collision)
      case (Some(speed), Some(angularSpeed)) =>
        resolveBounceAndRotation(entity, other, speed, angularSpeed, collision)
      case (None, Some(angularSpeed)) =>
        resolveRotation(entity, other, angularSpeed, collision)
      case (None, None) =>
        Right(entity)

  private def resolveSimpleBounce(
      entity: Entity,
      other: Entity,
      entitySpeed: Vector2D,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    val reflectedSpeed =
      other.speed match
        case None =>
          Right(
            PhysicsUtil.reflectOnFixed(
              entitySpeed,
              collision.normalVector
            )
          )
        case Some(otherSpeed) =>
          PhysicsUtil.reflectOnMobile(
            entitySpeed,
            otherSpeed,
            collision.normalVector,
            entity.weight,
            other.weight
          )

    reflectedSpeed.map(entity.withSpeed)
    
  private def resolveBounceAndRotation(
      entity: Entity,
      other: Entity,
      entitySpeed: Vector2D,
      angularSpeed: Double,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    Right(entity)

  private def resolveRotation(
      entity: Entity,
      other: Entity,
      angularSpeed: Double,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    Right(entity)  
