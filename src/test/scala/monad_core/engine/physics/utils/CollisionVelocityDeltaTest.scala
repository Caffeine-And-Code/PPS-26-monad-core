package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeFixedEntityRectangle,
  makeMovingEntityCircle,
  makeMovingEntityRectangle
}
import monad_core.engine.model.*
import monad_core.engine.physics.core.ZeroMassError
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollisionVelocityDeltaTest extends AnyFunSuite with Matchers:

  private val PointSpeed: (Entity, Vector2D) => Vector2D = (entity, point) =>
    val linearSpeed = entity.speed.getOrElse(Vector2D(0.0, 0.0))
    val radius      = point - entity.position
    val rotationSpeed = entity.angularSpeed
      .map(angularSpeed => Vector2D(-radius.y, radius.x) * math.toRadians(angularSpeed))
      .getOrElse(Vector2D(0.0, 0.0))
    linearSpeed + rotationSpeed

  private val MomentOfInertia: (Shape2D, Double) => Double = (shape, mass) =>
    CollisionVelocityDelta.momentOfInertia(shape, mass)

  private val AngularInverseMass: (Vector2D, Vector2D, Double) => Double =
    (radius, normal, inertia) =>
      val leverArm = radius cross normal
      leverArm * leverArm / inertia

  private val Impulse: (Double, Double) => Double = (incomingSpeed, totalInverseMass) =>
    PhysicsUtil.computeImpulse(incomingSpeed, totalInverseMass)

  private val AngularSpeedChange: (Vector2D, Vector2D, Double) => Double =
    (radius, impulseVector, inertia) => math.toDegrees((radius cross impulseVector) / inertia)

  private val KineticEnergy: (Double, Vector2D, Double, Double) => Double =
    (mass, speed, inertia, angularSpeed) =>
      val angularSpeedRadians = math.toRadians(angularSpeed)
      mass * speed.magnitude * speed.magnitude / 2.0 +
        inertia * angularSpeedRadians * angularSpeedRadians / 2.0

  extension (r: VelocityDelta)

    private def separate: (Vector2D, Double) =
      (r.speed, r.angularSpeed)

  private val Epsilon = 1e-9

  private val HorizontalCollision = Collision(
    normalVector = Vector2D(1.0, 0.0),
    penetrationDepth = 0.0,
    collisionPoint = Vector2D(0.0, 0.0)
  )

  test("speedAtPoint should combine linear and angular speed"):
    val collisionPoint = Vector2D(2.0, 1.0)
    val entity = makeMovingEntityCircle(
      position = Vector2D(1.0, 1.0),
      speed = Vector2D(2.0, 3.0)
    ).withAngularSpeed(Some(90.0))
    val expectedSpeed = PointSpeed(entity, collisionPoint)

    val result = CollisionVelocityDelta.speedAtPoint(entity, collisionPoint)

    result.x shouldBe expectedSpeed.x +- Epsilon
    result.y shouldBe expectedSpeed.y +- Epsilon

  test("this function should split an off-center impulse between translation and rotation"):
    val entity = makeFixedEntityRectangle(
      position = Vector2D(5.0, 5.0),
      width = 4.0,
      height = 2.0
    ).withSpeed(Some(Vector2D(-1.0, 0.0))).withAngularSpeed(Some(0.0)).withWeight(Some(1)).value

    val wall = makeFixedEntityRectangle(id = "wall")

    val collision = Collision(
      normalVector = Vector2D(1.0, 0.0),
      penetrationDepth = 0.0,
      collisionPoint = Vector2D(4.0, 7.0)
    )

    val mass    = PhysicsUtil.actualDoubleWeight(entity.weight).value
    val inertia = MomentOfInertia(entity.shape, mass)
    val radius  = collision.collisionPoint - entity.position
    val relativeSpeed =
      PointSpeed(entity, collision.collisionPoint) - PointSpeed(wall, collision.collisionPoint)
    val incomingSpeed = PhysicsUtil.incomingSpeedAlongNormal(relativeSpeed, collision.normalVector)
    val totalInverseMass =
      PhysicsUtil.inverseMass(mass) + AngularInverseMass(radius, collision.normalVector, inertia)
    val impulse                    = Impulse(incomingSpeed, totalInverseMass)
    val impulseVector              = collision.normalVector * impulse
    val expectedSpeedChange        = impulseVector * PhysicsUtil.inverseMass(mass)
    val expectedAngularSpeedChange = AngularSpeedChange(radius, impulseVector, inertia)

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, wall, collision).value.separate

    speedChange.x shouldBe expectedSpeedChange.x +- Epsilon
    speedChange.y shouldBe expectedSpeedChange.y +- Epsilon
    angularSpeedChange shouldBe expectedAngularSpeedChange +- Epsilon

  test("speedAtPoint should return linear speed at the center of mass"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(2.0, 3.0),
      speed = Vector2D(4.0, 5.0)
    ).withAngularSpeed(Some(180.0))

    CollisionVelocityDelta.speedAtPoint(entity, entity.position) shouldBe Vector2D(4.0, 5.0)

  test("speedAtPoint should return zero for a fixed entity"):
    val entity = makeFixedEntityCircle(position = Vector2D(2.0, 3.0))

    CollisionVelocityDelta.speedAtPoint(entity, Vector2D(10.0, 10.0)) shouldBe Vector2D(0.0, 0.0)

  test("speedAtPoint should compute speed for a rotation-only entity"):
    val entity         = makeFixedEntityCircle().withAngularSpeed(Some(90.0))
    val collisionPoint = Vector2D(0.0, 2.0)
    val expectedSpeed  = PointSpeed(entity, collisionPoint)

    val result = CollisionVelocityDelta.speedAtPoint(entity, collisionPoint)

    result.x shouldBe expectedSpeed.x +- Epsilon
    result.y shouldBe expectedSpeed.y +- Epsilon

  test("speedAtPoint should respect clockwise rotation"):
    val entity         = makeFixedEntityCircle().withAngularSpeed(Some(-90.0))
    val collisionPoint = Vector2D(2.0, 0.0)
    val expectedSpeed  = PointSpeed(entity, collisionPoint)

    val result = CollisionVelocityDelta.speedAtPoint(entity, collisionPoint)

    result.x shouldBe expectedSpeed.x +- Epsilon
    result.y shouldBe expectedSpeed.y +- Epsilon

  test("this function should return an error when entity mass is missing"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0)).withAngularSpeed(Some(0.0))
    val wall   = makeFixedEntityRectangle(id = "wall")

    CollisionVelocityDelta(entity, wall, HorizontalCollision) shouldBe Left(ZeroMassError())

  test("this function should return no impulse for separating contact points"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(1.0, 0.0))
      .withWeight(Some(1))
      .value
      .withAngularSpeed(Some(0.0))

    val wall = makeFixedEntityRectangle(id = "wall")

    CollisionVelocityDelta(entity, wall, HorizontalCollision).value.separate shouldBe
      (Vector2D(0.0, 0.0), 0.0)

  test("this function should not create torque for a centered impulse"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(Some(1)).value.withAngularSpeed(Some(0.0))

    val wall = makeFixedEntityRectangle(id = "wall")

    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(-1.0, 0.0))

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, wall, collision).value.separate

    speedChange shouldBe Vector2D(2.0, 0.0)
    angularSpeedChange shouldBe 0.0

  test("this function should respect a locked translation degree of freedom"):
    val entity = makeFixedEntityRectangle()
      .withWeight(Some(1))
      .value
      .withAngularSpeed(Some(0.0))

    val other = makeMovingEntityCircle(
      id = "other",
      speed = Vector2D(1.0, 0.0)
    ).withWeight(Some(1)).value

    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 1.0))

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, other, collision).value.separate

    speedChange shouldBe Vector2D(0.0, 0.0)
    angularSpeedChange should not be 0.0

  test("this function should respect a locked rotation degree of freedom"):
    val entity    = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0)).withWeight(Some(1)).value
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 2.0))

    val (_, angularSpeedChange) = CollisionVelocityDelta(entity, wall, collision).value.separate

    angularSpeedChange shouldBe 0.0

  test("this function should support a fixed other entity without mass"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0))
      .withWeight(Some(1))
      .value
      .withAngularSpeed(Some(0.0))

    val wall = makeFixedEntityRectangle(id = "wall")

    CollisionVelocityDelta(entity, wall, HorizontalCollision).isRight shouldBe true

  test("this function should conserve kinetic energy against a fixed body"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(5.0, 5.0),
      width = 4.0,
      height = 2.0,
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(Some(1)).value.withAngularSpeed(Some(0.0))

    val wall = makeFixedEntityRectangle(id = "wall")

    val collision = Collision(Vector2D(1.0, 0.0), 0.0, Vector2D(4.0, 7.0))

    val mass           = PhysicsUtil.actualDoubleWeight(entity.weight).value
    val inertia        = MomentOfInertia(entity.shape, mass)
    val expectedEnergy = KineticEnergy(mass, entity.speed.value, inertia, entity.angularSpeed.value)

    val (speedChange, angularChange) =
      CollisionVelocityDelta(entity, wall, collision).value.separate

    val finalSpeed        = entity.speed.value + speedChange
    val finalAngularSpeed = entity.angularSpeed.value + angularChange
    val finalEnergy       = KineticEnergy(mass, finalSpeed, inertia, finalAngularSpeed)

    finalEnergy shouldBe expectedEnergy +- Epsilon

  test("this function should include both entities' translational inverse masses"):
    val entity = makeMovingEntityCircle(speed = Vector2D(-1.0, 0.0))
      .withWeight(Some(1))
      .value

    val other = makeMovingEntityCircle(id = "other", speed = Vector2D(1.0, 0.0))
      .withWeight(Some(1))
      .value

    val (speedChange, _) =
      CollisionVelocityDelta(entity, other, HorizontalCollision).value.separate

    speedChange shouldBe Vector2D(2.0, 0.0)

  test("this function should not add inverse mass for locked entity translation"):
    val radius = 1.0

    val entity = makeFixedEntityCircle(radius = radius)
      .withAngularSpeed(Some(0.0))
      .withWeight(Some(1))
      .value

    val other = makeMovingEntityCircle(
      id = "other",
      radius = radius,
      speed = Vector2D(1.0, 0.0)
    )
      .withWeight(Some(1))
      .value

    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val mass            = PhysicsUtil.actualDoubleWeight(entity.weight).value
    val otherMass       = PhysicsUtil.actualDoubleWeight(other.weight).value
    val inertia         = MomentOfInertia(entity.shape, mass)
    val collisionRadius = collision.collisionPoint - entity.position
    val relativeSpeed =
      PointSpeed(entity, collision.collisionPoint) - PointSpeed(other, collision.collisionPoint)
    val incomingSpeed = PhysicsUtil.incomingSpeedAlongNormal(relativeSpeed, collision.normalVector)
    val totalInverseMass =
      PhysicsUtil.inverseMass(otherMass) + AngularInverseMass(
        collisionRadius,
        collision.normalVector,
        inertia
      )
    val impulse                    = Impulse(incomingSpeed, totalInverseMass)
    val impulseVector              = collision.normalVector * impulse
    val expectedAngularSpeedChange = AngularSpeedChange(collisionRadius, impulseVector, inertia)

    val (_, angularSpeedChange) =
      CollisionVelocityDelta(entity, other, collision).value.separate

    angularSpeedChange shouldBe expectedAngularSpeedChange +- Epsilon

  test("this function should not add rotational inverse mass for locked entity rotation"):
    val radius = 1.0

    val entity = makeMovingEntityCircle(radius = radius, speed = Vector2D(-1.0, 0.0))
      .withWeight(Some(1))
      .value

    val other = makeFixedEntityCircle(id = "other", radius = radius)

    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, other, collision).value.separate

    speedChange shouldBe Vector2D(2.0, 0.0)
    angularSpeedChange shouldBe 0.0

  test("this function should include the other entity's rotational inverse mass"):
    val radius = 1.0

    val entity = makeMovingEntityCircle(radius = radius, speed = Vector2D(-1.0, 0.0))
      .withWeight(Some(1))
      .value

    val other = makeFixedEntityCircle(id = "other", radius = radius)
      .withAngularSpeed(Some(0.0))
      .withWeight(Some(1))
      .value

    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val mass         = PhysicsUtil.actualDoubleWeight(entity.weight).value
    val otherMass    = PhysicsUtil.actualDoubleWeight(other.weight).value
    val otherInertia = MomentOfInertia(other.shape, otherMass)
    val otherRadius  = collision.collisionPoint - other.position
    val relativeSpeed =
      PointSpeed(entity, collision.collisionPoint) - PointSpeed(other, collision.collisionPoint)
    val incomingSpeed = PhysicsUtil.incomingSpeedAlongNormal(relativeSpeed, collision.normalVector)
    val totalInverseMass =
      PhysicsUtil
        .inverseMass(mass) + AngularInverseMass(otherRadius, collision.normalVector, otherInertia)
    val impulse             = Impulse(incomingSpeed, totalInverseMass)
    val expectedSpeedChange = collision.normalVector * impulse * PhysicsUtil.inverseMass(mass)

    val (speedChange, _) = CollisionVelocityDelta(entity, other, collision).value.separate

    speedChange.x shouldBe expectedSpeedChange.x +- Epsilon
    speedChange.y shouldBe expectedSpeedChange.y

  test("this function should return zero when all effective inverse masses are zero"):
    val entity = makeFixedEntityCircle()
      .withAngularSpeed(Some(0.0))
      .withWeight(Some(1))
      .value

    val other = makeFixedEntityCircle(id = "other")

    CollisionVelocityDelta(entity, other, HorizontalCollision).value.separate shouldBe
      (Vector2D(0.0, 0.0) -> 0.0)
