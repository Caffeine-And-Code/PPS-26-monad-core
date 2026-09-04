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
    (entity.speed, entity.angularSpeed, other.angularSpeed) match
      case (None, None, _) =>
        Right(entity)

      case (Some(speed), None, None) =>
        resolveBasicBounce(entity, other, speed, collision)

      case _ =>
        CollisionVelocityDelta(entity, other, collision)
          .map(applyVelocityDelta(entity, _))

  private def resolveBasicBounce(
      entity: Entity,
      other: Entity,
      entitySpeed: Vector2D,
      collision: Collision
  ): Either[PhysicsError, Entity] =
    val newSpeed = other.speed match
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

    newSpeed.map(speed => entity.withSpeed(Some(speed)))

  private def applyVelocityDelta(
      entity: Entity,
      delta: VelocityDelta
  ): Entity =
    val withLinearDelta =
      entity.speed.fold(entity): speed =>
        entity.withSpeed(Some(speed + delta.speed))

    entity.angularSpeed.fold(withLinearDelta): angularSpeed =>
      withLinearDelta.withAngularSpeed(Some(angularSpeed + delta.angularSpeed))
