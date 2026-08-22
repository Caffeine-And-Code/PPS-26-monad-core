package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsError, ZeroMassError}
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeFixedEntityRectangle,
  makeMovingEntityCircle
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollisionResolverTest extends AnyFunSuite with Matchers:

  private def expectedPositionMobileFixed(
      mobile: Entity,
      collision: Collision
  ): Vector2D =
    PhysicsUtil.pushMobileOverlappingFixed(
      mobile.position,
      collision.normalVector,
      collision.penetrationDepth
    )

  private def expectedPositionMobileMobile(
      mobile1: Entity,
      mobile2: Entity,
      collision: Collision
  ): Either[PhysicsError, Vector2D] =
    PhysicsUtil.pushMobileOverlappingMobile(
      mobile1.position,
      collision.normalVector,
      collision.penetrationDepth,
      mobile1.weight,
      mobile2.weight
    )

  private def expectedSpeedMobileFixed(
      mobile: Entity,
      collision: Collision
  ): Vector2D =
    PhysicsUtil.reflectOnFixed(
      mobile.speed.value,
      collision.normalVector
    )

  private def expectedSpeedMobileMobile(
      mobile1: Entity,
      mobile2: Entity,
      collision: Collision
  ): Either[PhysicsError, Vector2D] =
    PhysicsUtil.reflectOnMobile(
      mobile1.speed.value,
      mobile2.speed.value,
      collision.normalVector,
      mobile1.weight,
      mobile2.weight
    )

  test("the function should return an empty list when the input CollisionMap is empty"):
    val collisions: CollisionMap = Map.empty

    val result = CollisionResolver(collisions)

    result shouldBe Right(List.empty)

  test("the function should not return an entity when the updated one is fixed"):

    val fixedEntity = makeFixedEntityCircle(id = "fixed")
    val otherEntity = makeMovingEntityCircle(id = "moving")
    val collision   = Collision(Vector2D(1, 0), 1.0, Vector2D(0, 0))

    val collisions: CollisionMap = Map(fixedEntity -> List((otherEntity, collision)))

    val result = CollisionResolver(collisions)

    result shouldBe Right(List.empty)

  test(
    "the function should return an error when resolving a collision with one mobile entity without mass"
  ):

    val entity1   = makeMovingEntityCircle(id = "entity1")
    val entity2   = makeMovingEntityCircle(id = "entity2").withWeight(1).value
    val collision = Collision(Vector2D(1, 0), 1.0, Vector2D(0, 0))

    val collisions: CollisionMap = Map(entity1 -> List((entity2, collision)))

    val result = CollisionResolver(collisions)

    result shouldBe Left(ZeroMassError())

  test(
    "the function should return an error when resolving a collision with two mobiles entities without mass"
  ):

    val entity1   = makeMovingEntityCircle(id = "entity1")
    val entity2   = makeMovingEntityCircle(id = "entity2")
    val collision = Collision(Vector2D(1, 0), 1.0, Vector2D(0, 0))

    val collisions: CollisionMap = Map(entity1 -> List((entity2, collision)))

    val result = CollisionResolver(collisions)

    result shouldBe Left(ZeroMassError())

  test(
    "the function should return a list containing all updated entities when resolving a collision with one mobile entity and one fixed entity"
  ):

    val speed = Vector2D(1, 0)

    val entity1   = makeMovingEntityCircle(id = "entity1", speed = speed)
    val entity2   = makeFixedEntityCircle(id = "entity2")
    val collision = Collision(Vector2D(1, 0), 1.0, Vector2D(0, 0))

    val expectedPosition = expectedPositionMobileFixed(entity1, collision)

    val expectedSpeed = expectedSpeedMobileFixed(entity1, collision)

    val collisions: CollisionMap = Map(entity1 -> List((entity2, collision)))

    val result = CollisionResolver(collisions).value

    val resultEntity = result.find(_.id == entity1.id).value

    resultEntity.position shouldBe expectedPosition
    resultEntity.speed shouldBe Some(expectedSpeed)
    resultEntity.angularSpeed shouldBe None

  test(
    "the function should return a list containing all updated entities when resolving a collision with two mobile entities"
  ):

    val entity1 = makeMovingEntityCircle(id = "entity1", speed = Vector2D(1, 0)).withWeight(1).value
    val entity2 =
      makeMovingEntityCircle(id = "entity2", speed = Vector2D(-1, 0)).withWeight(1).value
    val collision1 = Collision(Vector2D(-1, 0), 1.0, Vector2D(0, 0))
    val collision2 = Collision(Vector2D(1, 0), 1.0, Vector2D(0, 0))

    val expectedPosition1 = expectedPositionMobileMobile(entity1, entity2, collision1).value

    val expectedPosition2 = expectedPositionMobileMobile(entity2, entity1, collision2).value

    val expectedSpeed1 = expectedSpeedMobileMobile(entity1, entity2, collision1).value

    val expectedSpeed2 = expectedSpeedMobileMobile(entity2, entity1, collision2).value

    val collisions: CollisionMap = Map(
      entity1 -> List((entity2, collision1)),
      entity2 -> List((entity1, collision2))
    )

    val result = CollisionResolver(collisions).value

    val resultEntity1 = result.find(_.id == entity1.id).value
    val resultEntity2 = result.find(_.id == entity2.id).value

    resultEntity1.position shouldBe expectedPosition1
    resultEntity2.position shouldBe expectedPosition2
    resultEntity1.speed.value shouldBe expectedSpeed1
    resultEntity2.speed.value shouldBe expectedSpeed2

  test(
    "the function should return a list containing all updated entities when resolving multiple collisions"
  ):

    val entity1 = makeMovingEntityCircle(id = "entity1", speed = Vector2D(1, 0)).withWeight(1).value
    val entity2 =
      makeMovingEntityCircle(id = "entity2", speed = Vector2D(-1, 0)).withWeight(1).value
    val entity3     = makeFixedEntityCircle(id = "entity3")
    val collision12 = Collision(Vector2D(-1, 0), 1.0, Vector2D(0, 0))
    val collision21 = Collision(Vector2D(1, 0), 1.0, Vector2D(0, 0))
    val collision13 = Collision(Vector2D(0, 1), 1.0, Vector2D(0, 0))

    val expectedPositionAfter12 = expectedPositionMobileMobile(entity1, entity2, collision12).value
    val expectedSpeedAfter12    = expectedSpeedMobileMobile(entity1, entity2, collision12).value
    val entityAfter1 = entity1.moveTo(expectedPositionAfter12).withSpeed(expectedSpeedAfter12)

    val expectedPosition1 = expectedPositionMobileFixed(entityAfter1, collision13)
    val expectedSpeed1    = expectedSpeedMobileFixed(entityAfter1, collision13)

    val expectedPosition2 = expectedPositionMobileMobile(entity2, entity1, collision21).value
    val expectedSpeed2    = expectedSpeedMobileMobile(entity2, entity1, collision21).value

    val collisions: CollisionMap = Map(
      entity1 -> List((entity2, collision12), (entity3, collision13)),
      entity2 -> List((entity1, collision21))
    )

    val result = CollisionResolver(collisions).value

    val resultEntity1 = result.find(_.id == entity1.id).value
    val resultEntity2 = result.find(_.id == entity2.id).value

    resultEntity1.position shouldBe expectedPosition1
    resultEntity1.speed.value shouldBe expectedSpeed1
    resultEntity2.position shouldBe expectedPosition2
    resultEntity2.speed.value shouldBe expectedSpeed2

  test("a rotation-only entity should not be translated while resolving overlap"):
    val rotating = makeFixedEntityRectangle(id = "rotating")
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)

    val wall = makeFixedEntityRectangle(id = "wall")

    val collision = Collision(Vector2D(1.0, 0.0), 2.0, rotating.position)

    val result = CollisionResolver(Map(rotating -> List(wall -> collision))).value.head

    result.position shouldBe rotating.position

  test("a mobile entity should resolve the full overlap against a rotation-only entity"):
    val mobile = makeMovingEntityCircle(
      id = "mobile",
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(0.0, 0.0)
    ).withWeight(1).value

    val rotating = makeFixedEntityRectangle(id = "rotating")
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)

    val collision = Collision(Vector2D(-1.0, 0.0), 2.0, Vector2D(0.0, 0.0))

    val result = CollisionResolver(Map(mobile -> List(rotating -> collision))).value.head

    result.position shouldBe Vector2D(-2.0, 0.0)

  test("mobile overlap correction should move a heavier entity less"):
    val heavy = makeMovingEntityCircle(
      id = "heavy",
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(0.0, 0.0)
    ).withWeight(3).value

    val light = makeMovingEntityCircle(
      id = "light",
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(0.0, 0.0)
    ).withWeight(1).value

    val heavyCollision = Collision(Vector2D(-1.0, 0.0), 4.0, Vector2D(0.0, 0.0))
    val lightCollision = heavyCollision.copy(normalVector = Vector2D(1.0, 0.0))

    val collisions = Map(
      heavy -> List(light -> heavyCollision),
      light -> List(heavy -> lightCollision)
    )

    val result       = CollisionResolver(collisions).value
    val updatedHeavy = result.find(_.id == heavy.id).value
    val updatedLight = result.find(_.id == light.id).value

    updatedHeavy.position shouldBe Vector2D(-1.0, 0.0)
    updatedLight.position shouldBe Vector2D(3.0, 0.0)

  test("zero penetration should not change entity position"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(3.0, 4.0),
      speed = Vector2D(1.0, 0.0)
    )

    val wall = makeFixedEntityRectangle(id = "wall")

    val collision = Collision(Vector2D(1.0, 0.0), 0.0, Vector2D(3.0, 4.0))

    val result = CollisionResolver(Map(entity -> List(wall -> collision))).value.head

    result.position shouldBe entity.position

  test("overlap correction should follow each collision normal"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(0.0, 0.0)
    )

    val horizontalWall = makeFixedEntityRectangle(id = "horizontal")

    val verticalWall = makeFixedEntityRectangle(id = "vertical")

    val collisions = List(
      horizontalWall -> Collision(Vector2D(1.0, 0.0), 2.0, Vector2D(0.0, 0.0)),
      verticalWall   -> Collision(Vector2D(0.0, -1.0), 3.0, Vector2D(0.0, 0.0))
    )

    val result = CollisionResolver(Map(entity -> collisions)).value.head

    result.position shouldBe Vector2D(2.0, -3.0)

  test("collision resolution should preserve unrelated entity properties"):
    val entity = makeMovingEntityCircle(speed = Vector2D(1.0, 0.0))
      .withHealth(80)
      .value
      .withTeamId("team")
      .value

    val wall = makeFixedEntityRectangle(id = "wall")

    val collision = Collision(Vector2D(1.0, 0.0), 1.0, Vector2D(0.0, 0.0))

    val result = CollisionResolver(Map(entity -> List(wall -> collision))).value.head

    result.health shouldBe entity.health
    result.teamId shouldBe entity.teamId
