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

class BounceResponseTest extends AnyFunSuite with Matchers:

  private val HorizontalCollision = Collision(
    normalVector = Vector2D(1.0, 0.0),
    penetrationDepth = 0.0,
    collisionPoint = Vector2D(0.0, 0.0)
  )

  test("a fixed entity should remain unchanged"):
    val entity = makeFixedEntityCircle(id = "fixed")
    val other  = makeMovingEntityCircle(id = "moving", speed = Vector2D(-1.0, 0.0))

    BounceResponse(entity, other, HorizontalCollision).value shouldBe entity

  test("a linear entity should bounce against a fixed entity"):
    val entity = makeMovingEntityCircle(speed = Vector2D(-2.0, 1.0))
    val wall   = makeFixedEntityRectangle(id = "wall")

    val result = BounceResponse(entity, wall, HorizontalCollision).value

    result.speed.value shouldBe Vector2D(2.0, 1.0)

  test("a linear entity should preserve its speed when separating from a fixed entity"):
    val entity = makeMovingEntityCircle(speed = Vector2D(2.0, 1.0))
    val wall   = makeFixedEntityRectangle(id = "wall")

    BounceResponse(entity, wall, HorizontalCollision).value.speed shouldBe entity.speed

  test("two linear entities with equal masses should exchange normal speeds"):
    val entity = makeMovingEntityCircle(speed = Vector2D(-1.0, 2.0)).withWeight(1).value
    val other = makeMovingEntityCircle(
      id = "other",
      speed = Vector2D(1.0, 3.0)
    ).withWeight(1).value

    val result = BounceResponse(entity, other, HorizontalCollision).value

    result.speed.value shouldBe Vector2D(1.0, 2.0)

  test("a linear collision between mobile entities should require both masses"):
    val entity = makeMovingEntityCircle(speed = Vector2D(-1.0, 0.0))
    val other = makeMovingEntityCircle(
      id = "other",
      speed = Vector2D(1.0, 0.0)
    ).withWeight(1).value

    BounceResponse(entity, other, HorizontalCollision) shouldBe Left(ZeroMassError())

  test("a rotating other entity should affect a linear entity through contact speed"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(1.0, 0.0)
    ).withWeight(1).value
    val other = makeFixedEntityRectangle(
      id = "rotating",
      position = Vector2D(2.0, 0.0),
      width = 2.0,
      height = 2.0
    ).withWeight(1).value.withAngularSpeed(360.0)
    val collision = Collision(
      normalVector = Vector2D(-1.0, 0.0),
      penetrationDepth = 0.0,
      collisionPoint = Vector2D(1.0, 1.0)
    )
    val expectedChange = PhysicsUtil.collisionResponse(entity, other, collision).value._1

    val result = BounceResponse(entity, other, collision).value

    result.speed.value shouldBe entity.speed.value + expectedChange

  test("an off-center collision should update linear and angular speeds"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(5.0, 5.0),
      width = 4.0,
      height = 2.0,
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(1).value.withAngularSpeed(0.0)
    val wall = makeFixedEntityCircle(id = "wall")
    val collision = Collision(
      normalVector = Vector2D(1.0, 0.0),
      penetrationDepth = 0.0,
      collisionPoint = Vector2D(4.0, 7.0)
    )
    val (linearChange, angularChange) =
      PhysicsUtil.collisionResponse(entity, wall, collision).value

    val result = BounceResponse(entity, wall, collision).value

    result.speed.value shouldBe entity.speed.value + linearChange
    result.angularSpeed.value shouldBe angularChange

  test("a centered collision should not change angular speed"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(-2.0, 0.0)
    ).withWeight(1).value.withAngularSpeed(15.0)
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(-1.0, 0.0))

    val result = BounceResponse(entity, wall, collision).value

    result.speed.value.x shouldBe 2.0 +- 1e-9
    result.angularSpeed.value shouldBe 15.0 +- 1e-9

  test("a rotation-only entity should change angular speed without acquiring linear speed"):
    val entity = makeFixedEntityRectangle(
      position = Vector2D(0.0, 0.0),
      width = 2.0,
      height = 2.0
    ).withWeight(1).value.withAngularSpeed(0.0)
    val other = makeMovingEntityCircle(
      id = "other",
      position = Vector2D(-2.0, 0.0),
      speed = Vector2D(1.0, 0.0)
    ).withWeight(1).value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 1.0))

    val result = BounceResponse(entity, other, collision).value

    result.position shouldBe entity.position
    result.speed shouldBe None
    result.angularSpeed.value should not be 0.0

  test("a rotation-only entity should remain unchanged when contact points are separating"):
    val entity = makeFixedEntityRectangle()
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)
    val other = makeMovingEntityCircle(
      id = "other",
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(1).value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 1.0))

    BounceResponse(entity, other, collision).value shouldBe entity

  test("a rotating entity should require a mass"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0)).withAngularSpeed(0.0)
    val wall   = makeFixedEntityRectangle(id = "wall")

    BounceResponse(entity, wall, HorizontalCollision) shouldBe Left(ZeroMassError())

  test("bounce response should preserve position and rotation"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(3.0, 4.0),
      speed = Vector2D(-1.0, 0.0),
      rotation = 25.0
    ).withWeight(2).value.withAngularSpeed(10.0)
    val wall = makeFixedEntityRectangle(id = "wall")

    val result = BounceResponse(entity, wall, HorizontalCollision).value

    result.position shouldBe entity.position
    result.rotation shouldBe entity.rotation
