package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeFixedEntityRectangle,
  makeMovingEntityCircle,
  makeMovingEntityRectangle
}
import monad_core.engine.model.{+, Vector2D, magnitude}
import monad_core.engine.physics.core.ZeroMassError
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollisionVelocityDeltaTest extends AnyFunSuite with Matchers:

  extension (r: VelocityDelta)

    private def separate: (Vector2D, Double) =
      (r.speed, r.angularSpeed)

  private val HorizontalCollision = Collision(
    normalVector = Vector2D(1.0, 0.0),
    penetrationDepth = 0.0,
    collisionPoint = Vector2D(0.0, 0.0)
  )

  test("speedAtPoint should combine linear and angular speed"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(1.0, 1.0),
      speed = Vector2D(2.0, 3.0)
    ).withAngularSpeed(90.0)

    val result = CollisionVelocityDelta.speedAtPoint(entity, Vector2D(2.0, 1.0))

    result.x shouldBe 2.0 +- 1e-9
    result.y shouldBe (3.0 + math.Pi / 2.0) +- 1e-9

  test("collisionResponse should split an off-center impulse between translation and rotation"):
    val entity = makeFixedEntityRectangle(
      position = Vector2D(5.0, 5.0),
      width = 4.0,
      height = 2.0
    ).withSpeed(Vector2D(-1.0, 0.0)).withAngularSpeed(0.0).withWeight(1).value
    val wall = makeFixedEntityRectangle(id = "wall")
    val collision = Collision(
      normalVector = Vector2D(1.0, 0.0),
      penetrationDepth = 0.0,
      collisionPoint = Vector2D(4.0, 7.0)
    )

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, wall, collision).value.separate

    val inertia = (4.0 * 4.0 + 2.0 * 2.0) / 12.0
    val impulse = 2.0 / 3.4

    speedChange.x shouldBe impulse +- 1e-9
    speedChange.y shouldBe 0.0 +- 1e-9
    angularSpeedChange shouldBe math.toDegrees(-2.0 * impulse / inertia) +- 1e-9

  test("speedAtPoint should return linear speed at the center of mass"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(2.0, 3.0),
      speed = Vector2D(4.0, 5.0)
    ).withAngularSpeed(180.0)

    CollisionVelocityDelta.speedAtPoint(entity, entity.position) shouldBe Vector2D(4.0, 5.0)

  test("speedAtPoint should return zero for a fixed entity"):
    val entity = makeFixedEntityCircle(position = Vector2D(2.0, 3.0))

    CollisionVelocityDelta.speedAtPoint(entity, Vector2D(10.0, 10.0)) shouldBe Vector2D(0.0, 0.0)

  test("speedAtPoint should compute speed for a rotation-only entity"):
    val entity = makeFixedEntityCircle().withAngularSpeed(90.0)

    val result = CollisionVelocityDelta.speedAtPoint(entity, Vector2D(0.0, 2.0))

    result.x shouldBe -math.Pi +- 1e-9
    result.y shouldBe 0.0 +- 1e-9

  test("speedAtPoint should respect clockwise rotation"):
    val entity = makeFixedEntityCircle().withAngularSpeed(-90.0)

    val result = CollisionVelocityDelta.speedAtPoint(entity, Vector2D(2.0, 0.0))

    result.x shouldBe 0.0 +- 1e-9
    result.y shouldBe -math.Pi +- 1e-9

  test("collisionResponse should return an error when entity mass is missing"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0)).withAngularSpeed(0.0)
    val wall   = makeFixedEntityRectangle(id = "wall")

    CollisionVelocityDelta(entity, wall, HorizontalCollision) shouldBe Left(ZeroMassError())

  test("collisionResponse should return no impulse for separating contact points"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(1.0, 0.0))
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)
    val wall = makeFixedEntityRectangle(id = "wall")

    CollisionVelocityDelta(entity, wall, HorizontalCollision).value.separate shouldBe
      (Vector2D(0.0, 0.0), 0.0)

  test("collisionResponse should not create torque for a centered impulse"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(1).value.withAngularSpeed(0.0)
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(-1.0, 0.0))

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, wall, collision).value.separate

    speedChange shouldBe Vector2D(2.0, 0.0)
    angularSpeedChange shouldBe 0.0

  test("collisionResponse should respect a locked translation degree of freedom"):
    val entity = makeFixedEntityRectangle()
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)
    val other = makeMovingEntityCircle(
      id = "other",
      speed = Vector2D(1.0, 0.0)
    ).withWeight(1).value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 1.0))

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, other, collision).value.separate

    speedChange shouldBe Vector2D(0.0, 0.0)
    angularSpeedChange should not be 0.0

  test("collisionResponse should respect a locked rotation degree of freedom"):
    val entity    = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0)).withWeight(1).value
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 2.0))

    val (_, angularSpeedChange) = CollisionVelocityDelta(entity, wall, collision).value.separate

    angularSpeedChange shouldBe 0.0

  test("collisionResponse should support a fixed other entity without mass"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)
    val wall = makeFixedEntityRectangle(id = "wall")

    CollisionVelocityDelta(entity, wall, HorizontalCollision).isRight shouldBe true

  test("collisionResponse should conserve kinetic energy against a fixed body"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(5.0, 5.0),
      width = 4.0,
      height = 2.0,
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(1).value.withAngularSpeed(0.0)
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = Collision(Vector2D(1.0, 0.0), 0.0, Vector2D(4.0, 7.0))
    val (speedChange, angularChange) =
      CollisionVelocityDelta(entity, wall, collision).value.separate
    val finalSpeed        = entity.speed.value + speedChange
    val finalAngularSpeed = math.toRadians(angularChange)
    val inertia           = (4.0 * 4.0 + 2.0 * 2.0) / 12.0
    val finalEnergy =
      finalSpeed.magnitude * finalSpeed.magnitude / 2.0 +
        inertia * finalAngularSpeed * finalAngularSpeed / 2.0

    finalEnergy shouldBe 0.5 +- 1e-9

  test("collisionResponse should include both entities' translational inverse masses"):
    val entity = makeMovingEntityCircle(speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
    val other = makeMovingEntityCircle(id = "other", speed = Vector2D(1.0, 0.0))
      .withWeight(1)
      .value

    val (speedChange, _) =
      CollisionVelocityDelta(entity, other, HorizontalCollision).value.separate

    speedChange shouldBe Vector2D(2.0, 0.0)

  test("collisionResponse should not add inverse mass for locked entity translation"):
    val radius = 1.0
    val entity = makeFixedEntityCircle(radius = radius)
      .withAngularSpeed(0.0)
      .withWeight(1)
      .value
    val other = makeMovingEntityCircle(
      id = "other",
      radius = radius,
      speed = Vector2D(1.0, 0.0)
    )
      .withWeight(1)
      .value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val (_, angularSpeedChange) =
      CollisionVelocityDelta(entity, other, collision).value.separate
    val inertia         = radius * radius / 2.0
    val expectedImpulse = 2.0 / (1.0 + radius * radius / inertia)

    angularSpeedChange shouldBe math.toDegrees(-radius * expectedImpulse / inertia) +- 1e-9

  test("collisionResponse should not add rotational inverse mass for locked entity rotation"):
    val radius = 1.0
    val entity = makeMovingEntityCircle(radius = radius, speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
    val other     = makeFixedEntityCircle(id = "other", radius = radius)
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val (speedChange, angularSpeedChange) =
      CollisionVelocityDelta(entity, other, collision).value.separate

    speedChange shouldBe Vector2D(2.0, 0.0)
    angularSpeedChange shouldBe 0.0

  test("collisionResponse should include the other entity's rotational inverse mass"):
    val radius = 1.0
    val entity = makeMovingEntityCircle(radius = radius, speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
    val other = makeFixedEntityCircle(id = "other", radius = radius)
      .withAngularSpeed(0.0)
      .withWeight(1)
      .value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val (speedChange, _) = CollisionVelocityDelta(entity, other, collision).value.separate
    val inertia          = radius * radius / 2.0
    val expectedImpulse  = 2.0 / (1.0 + radius * radius / inertia)

    speedChange.x shouldBe expectedImpulse +- 1e-9
    speedChange.y shouldBe 0.0

  test("collisionResponse should return zero when all effective inverse masses are zero"):
    val entity = makeFixedEntityCircle()
      .withAngularSpeed(0.0)
      .withWeight(1)
      .value
    val other = makeFixedEntityCircle(id = "other")

    CollisionVelocityDelta(entity, other, HorizontalCollision).value.separate shouldBe
      (Vector2D(0.0, 0.0) -> 0.0)
