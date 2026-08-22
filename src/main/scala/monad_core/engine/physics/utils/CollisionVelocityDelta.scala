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

private[utils] case class VelocityDelta(
    speed: Vector2D,
    angularSpeed: Double
)

private[utils] object CollisionVelocityDelta:

  private val VectorZero                   = Vector2D(0.0, 0.0)
  private val RotationRectangleCoefficient = 12.0

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

  private def effectiveInverseMass(
      speed: Option[Vector2D],
      mass: Double
  ): Double =
    if speed.isDefined then inverseMass(mass)
    else 0.0

  private def angularForce(
      angularSpeed: Option[Double],
      radius: Vector2D,
      normal: Vector2D,
      inertia: Double
  ): Double =
    if angularSpeed.isDefined then ((radius cross normal) ** 2) / inertia
    else 0.0

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

  def speedAtPoint(entity: Entity, point: Vector2D): Vector2D =
    val linearSpeed = entity.speed.getOrElse(VectorZero)
    val rotationSpeed = entity.angularSpeed
      .map: angularSpeed =>
        val radius = point - entity.position
        Vector2D(-radius.y, radius.x) * math.toRadians(angularSpeed)
      .getOrElse(VectorZero)

    linearSpeed + rotationSpeed

  def momentOfInertia(shape: Shape2D, mass: Double): Double =
    shape match
      case Shape2D.Circle(radius) => mass * radius * radius / 2.0
      case Shape2D.Rectangle(height, length) =>
        mass * (height * height + length * length) / RotationRectangleCoefficient
