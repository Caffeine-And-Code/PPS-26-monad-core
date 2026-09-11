package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.PhysicsError

/** Applies the linear and angular velocity response produced by one collision. */
private[physics] object BounceResponse:

  /**
   * Resolves the bounce of one entity against another.
   *
   * @param entity
   *   entity whose velocity is updated
   * @param other
   *   colliding entity
   * @param collision
   *   collision normal and contact information
   * @return
   *   updated entity, or a physics error when required mass is missing
   */
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

  /**
   * Resolves a collision that does not involve angular motion.
   *
   * It's used to permit basic collision responses in the absence of angular speed,
   * such as a ball bouncing on a wall.
   *
   * @param entity
   *   entity whose velocity is updated
   * @param other
   *   fixed or mobile colliding entity
   * @param entitySpeed
   *   current linear velocity
   * @param collision
   *   collision normal
   * @return
   *   entity with reflected linear velocity, or a missing-mass error
   */
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

  /**
   * Adds a calculated linear and angular delta to the supported entity velocities.
   *
   * @param entity
   *   entity to update
   * @param delta
   *   calculated linear and angular changes
   * @return
   *   entity with every supported velocity component updated
   */
  private def applyVelocityDelta(
      entity: Entity,
      delta: VelocityDelta
  ): Entity =
    val withLinearDelta =
      entity.speed.fold(entity): speed =>
        entity.withSpeed(Some(speed + delta.speed))

    entity.angularSpeed.fold(withLinearDelta): angularSpeed =>
      withLinearDelta.withAngularSpeed(Some(angularSpeed + delta.angularSpeed))
