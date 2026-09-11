package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.PhysicsError
import monad_core.engine.physics.utils.PhysicsUtil.{
  actualDoubleWeight,
  computeImpulse,
  incomingSpeedAlongNormal,
  inverseMass
}

/**
 * Linear and angular velocity changes produced by one collision impulse.
 *
 * @param speed
 *   linear velocity delta
 * @param angularSpeed
 *   angular velocity delta in degrees per second
 */
private[utils] case class VelocityDelta(
    speed: Vector2D,
    angularSpeed: Double
)

/** Calculates impulse-based velocity changes at a collision point. */
private[utils] object CollisionVelocityDelta:

  private val VectorZero                   = Vector2D(0.0, 0.0)
  private val RotationRectangleCoefficient = 12.0

  /**
   * Calculates the impulse-induced velocity delta applied to the first entity.
   * Linear and angular inverse-mass contributions are combined at the contact point.
   *
   * @param entity
   *   entity receiving the impulse
   * @param other
   *   other colliding entity
   * @param collision
   *   collision normal and contact point
   * @return
   *   linear and angular delta, or a missing-mass error
   */
  def apply(
      entity: Entity,
      other: Entity,
      collision: Collision
  ): Either[PhysicsError, VelocityDelta] =
    for mass <- actualDoubleWeight(entity.weight)
    yield
      val otherMass = other.weight.map(_.value.toDouble).getOrElse(1.0)

      val relativeSpeed =
        speedAtPoint(entity, collision.collisionPoint) -
          speedAtPoint(other, collision.collisionPoint)

      val incomingSpeed = incomingSpeedAlongNormal(relativeSpeed, collision.normalVector)

      val radius      = collision.collisionPoint - entity.position
      val otherRadius = collision.collisionPoint - other.position

      val inertia      = momentOfInertia(entity.shape, mass)
      val otherInertia = momentOfInertia(other.shape, otherMass)

      val totalInverseMass =
        aggregatedInverseMass(
          entity,
          other,
          mass,
          otherMass,
          inertia,
          otherInertia,
          radius,
          otherRadius,
          collision
        )

      val impulse = computeImpulse(
        incomingSpeed,
        totalInverseMass
      )

      val impulseVector = collision.normalVector * impulse

      val speedChange =
        if entity.speed.isDefined then impulseVector * inverseMass(mass)
        else VectorZero

      val angularSpeedChange =
        if entity.angularSpeed.isDefined then math.toDegrees((radius cross impulseVector) / inertia)
        else 0.0

      VelocityDelta(speedChange, angularSpeedChange)

  /**
   * Calculates the translational inverse-mass contribution of a body.
   *
   * @param speed
   *   optional velocity indicating whether translation is supported
   * @param mass
   *   validated body mass
   * @return
   *   inverse mass for a movable body, otherwise zero
   */
  private def effectiveInverseMass(
      speed: Option[Vector2D],
      mass: Double
  ): Double =
    if speed.isDefined then inverseMass(mass)
    else 0.0

  /**
   * Calculates the rotational inverse-mass contribution at a contact point.
   * The squared lever arm projected across the normal is divided by the moment of inertia.
   *
   * @param angularSpeed
   *   optional velocity indicating whether rotation is supported
   * @param radius
   *   vector from the center of mass to the contact point
   * @param normal
   *   collision normal
   * @param inertia
   *   moment of inertia
   * @return
   *   rotational contribution, otherwise zero for a non-rotating body
   */
  private def angularForce(
      angularSpeed: Option[Double],
      radius: Vector2D,
      normal: Vector2D,
      inertia: Double
  ): Double =
    if angularSpeed.isDefined then ((radius cross normal) ** 2) / inertia
    else 0.0

  /**
   * Aggregates translational and rotational inverse masses of both bodies.
   *
   * @param entity
   *   first colliding entity
   * @param other
   *   second colliding entity
   * @param mass
   *   first-body mass
   * @param otherMass
   *   second-body mass
   * @param inertia
   *   first-body moment of inertia
   * @param otherInertia
   *   second-body moment of inertia
   * @param radius
   *   first-body contact lever arm
   * @param otherRadius
   *   second-body contact lever arm
   * @param collision
   *   collision normal and contact data
   * @return
   *   effective inverse mass used by the collision impulse
   */
  private def aggregatedInverseMass(
      entity: Entity,
      other: Entity,
      mass: Double,
      otherMass: Double,
      inertia: Double,
      otherInertia: Double,
      radius: Vector2D,
      otherRadius: Vector2D,
      collision: Collision
  ): Double =

    val entityInverseMass = effectiveInverseMass(entity.speed, mass)
    val otherInverseMass  = effectiveInverseMass(other.speed, otherMass)
    val entityAngularForce =
      angularForce(entity.angularSpeed, radius, collision.normalVector, inertia)
    val otherAngularForce =
      angularForce(other.angularSpeed, otherRadius, collision.normalVector, otherInertia)

    entityInverseMass + otherInverseMass + entityAngularForce + otherAngularForce

  /**
   * Calculates an entity's velocity at a world-space point.
   * Tangential velocity from angular motion is added to the center's linear velocity.
   *
   * @param entity
   *   entity whose motion is evaluated
   * @param point
   *   world-space point
   * @return
   *   combined linear and tangential velocity at the point
   */
  def speedAtPoint(entity: Entity, point: Vector2D): Vector2D =
    val linearSpeed = entity.speed.getOrElse(VectorZero)
    val rotationSpeed = entity.angularSpeed
      .map: angularSpeed =>
        val radius = point - entity.position
        Vector2D(-radius.y, radius.x) * math.toRadians(angularSpeed)
      .getOrElse(VectorZero)

    linearSpeed + rotationSpeed

  /**
   * Calculates the planar moment of inertia for a supported solid shape.
   *
   * @param shape
   *   circle or rectangle geometry
   * @param mass
   *   body mass
   * @return
   *   moment of inertia around the shape center
   */
  def momentOfInertia(shape: Shape2D, mass: Double): Double =
    shape match
      case Shape2D.Circle(radius) => mass * radius * radius / 2.0
      case Shape2D.Rectangle(height, length) =>
        mass * (height * height + length * length) / RotationRectangleCoefficient
