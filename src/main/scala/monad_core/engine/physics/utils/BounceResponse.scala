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
        resolveBounce(entity, other, speed, collision)
      case (Some(speed), Some(angularSpeed)) =>
        resolveBounceAndRotation(entity, other, speed, angularSpeed, collision)
      case (None, Some(angularSpeed)) =>
        resolveRotation(entity, other, angularSpeed, collision)
      case _ =>
        Right(entity)

  private def resolveBounce(
      entity: Entity,
      other: Entity,
      entitySpeed: Vector2D,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    other.angularSpeed match
      case None =>
        val reflectedSpeed = resolveBasicBounce(
          entity,
          other,
          entitySpeed,
          collision
        )

        reflectedSpeed.map(entity.withSpeed)
      case _ =>
        PhysicsUtil
          .collisionResponse(entity, other, collision)
          .map((speedChange, _) => entity.withSpeed(entitySpeed + speedChange))

  private def resolveBasicBounce(
      entity: Entity,
      other: Entity,
      entitySpeed: Vector2D,
      collision: Collision
  ): Either[PhysicsError, Vector2D] =
    other.speed match
      case None =>
        Right(PhysicsUtil.reflectOnFixed(entitySpeed, collision.normalVector))
      case Some(otherSpeed) =>
        PhysicsUtil.reflectOnMobile(
          entitySpeed,
          otherSpeed,
          collision.normalVector,
          entity.weight,
          other.weight
        )

  private def resolveBounceAndRotation(
      entity: Entity,
      other: Entity,
      entitySpeed: Vector2D,
      angularSpeed: Double,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    PhysicsUtil
      .collisionResponse(entity, other, collision)
      .map:
        case (speedChange, angularSpeedChange) =>
          entity
            .withSpeed(entitySpeed + speedChange)
            .withAngularSpeed(angularSpeed + angularSpeedChange)

  private def resolveRotation(
      entity: Entity,
      other: Entity,
      angularSpeed: Double,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    PhysicsUtil
      .collisionResponse(entity, other, collision)
      .map((_, angularSpeedChange) => entity.withAngularSpeed(angularSpeed + angularSpeedChange))
