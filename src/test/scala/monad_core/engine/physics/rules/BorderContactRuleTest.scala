package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeMovingEntityCircle, makeMovingEntityRectangle}
import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.helper.{BorderContactHelper, MockDetectorHelper, MockSceneHelper}
import monad_core.engine.model.Vector2D
import monad_core.engine.physics.core.NegativeDeltaTime
import monad_core.engine.physics.pathfinding.SizeHelper
import monad_core.engine.physics.utils.BorderWallType
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BorderContactRuleTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with MockSceneHelper
    with MockDetectorHelper:

  private val Rule = BorderContactRule.borderContactRule

  private val UpperLeftBound  = Vector2D(0.0, 0.0)
  private val LowerRightBound = Vector2D(100.0, 100.0)

  given CollisionDetector = mock[CollisionDetector]

  private def testSingleWall(borderType: BorderWallType) =
    val defaultValues = BorderContactHelper.generateSingleWallEntities(
      borderType,
      UpperLeftBound,
      LowerRightBound
    )

    val (entity, wall, collision, expectedPosition, expectedSpeed) = defaultValues

    val scene = sceneWithEntities(List(entity))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (entity.id.value, wall.id.value) -> (
          collision.normalVector,
          collision.penetrationDepth,
          collision.collisionPoint
        )
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).get

    resultEntity.position shouldBe expectedPosition
    resultEntity.speed shouldBe Some(expectedSpeed)

  private def testCornerWall(borderTypeV: BorderWallType, borderTypeH: BorderWallType) =
    val data = BorderContactHelper.generateCornerEntities(
      borderTypeV,
      borderTypeH,
      UpperLeftBound,
      LowerRightBound
    )

    val entity           = data._1
    val leftWall         = data._2
    val leftCollision    = data._3
    val bottomWall       = data._4
    val bottomCollision  = data._5
    val expectedPosition = data._6
    val expectedSpeed    = data._7

    val scene = sceneWithEntities(List(entity))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (entity.id.value, leftWall.id.value) -> (
          leftCollision.normalVector,
          leftCollision.penetrationDepth,
          leftCollision.collisionPoint
        ),
        (entity.id.value, bottomWall.id.value) -> (
          bottomCollision.normalVector,
          bottomCollision.penetrationDepth,
          bottomCollision.collisionPoint
        )
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).get

    resultEntity.position shouldBe expectedPosition
    resultEntity.speed shouldBe Some(expectedSpeed)

  test("the rule should return NegativeDeltaTime when delta time is negative"):

    val mockScene = sceneWithEntities(List.empty)

    val result = Rule.apply(mockScene, NegativeDt)(using summon[CollisionDetector])

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when there are no entities"):
    val scene = sceneWithEntities(List())

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    result shouldBe scene

  test("the rule should not update a fixed entity, even if it is outside the scene borders"):

    val fixedEntity = makeFixedEntityCircle(
      position = Vector2D(-10, -10)
    )

    val scene = sceneWithEntities(List(fixedEntity))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == fixedEntity.id).get
    resultEntity.position shouldBe fixedEntity.position
    resultEntity.speed shouldBe fixedEntity.speed

  test("the rule should not update a moving entity when it is within the scene borders"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(50, 50),
      speed = Vector2D(1, 1)
    )

    val scene               = sceneWithEntities(List(entity))
    given CollisionDetector = detectorWithoutCollision()
    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).get
    resultEntity.position shouldBe entity.position
    resultEntity.speed shouldBe entity.speed

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on left"
  ):
    testSingleWall(BorderWallType.Left)

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on right"
  ):
    testSingleWall(BorderWallType.Right)

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on top"
  ):
    testSingleWall(BorderWallType.Top)

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on bottom"
  ):
    testSingleWall(BorderWallType.Bottom)

  test("the rule should push and bounce a moving entity when it is outside on left and top sides"):
    testCornerWall(BorderWallType.Left, BorderWallType.Top)

  test("the rule should push and bounce a moving entity when it is outside on right and top sides"):
    testCornerWall(BorderWallType.Right, BorderWallType.Top)

  test(
    "the rule should push and bounce a moving entity when it is outside on right and bottom sides"
  ):
    testCornerWall(BorderWallType.Right, BorderWallType.Bottom)

  test(
    "the rule should push and bounce a moving entity when it is outside on left and bottom sides"
  ):
    testCornerWall(BorderWallType.Left, BorderWallType.Bottom)

  test("the rule should update multiple entities when they are outside the scene borders"):

    val data1 = BorderContactHelper.generateSingleWallEntities(
      BorderWallType.Left,
      UpperLeftBound,
      LowerRightBound,
      entityId = "entity1"
    )

    val data2 = BorderContactHelper.generateSingleWallEntities(
      BorderWallType.Top,
      UpperLeftBound,
      LowerRightBound,
      entityId = "entity2"
    )

    val entity1    = data1._1
    val wall1      = data1._2
    val collision1 = data1._3

    val entity2    = data2._1
    val wall2      = data2._2
    val collision2 = data2._3

    val expectedPosition1 = data1._4
    val expectedSpeed1    = data1._5
    val expectedPosition2 = data2._4
    val expectedSpeed2    = data2._5

    val scene = sceneWithEntities(List(entity1, entity2))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (entity1.id.value, wall1.id.value) -> (
          collision1.normalVector,
          collision1.penetrationDepth,
          collision1.collisionPoint
        ),
        (entity2.id.value, wall2.id.value) -> (
          collision2.normalVector,
          collision2.penetrationDepth,
          collision2.collisionPoint
        )
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity1 = result.allEntities.find(_.id == entity1.id).get
    val resultEntity2 = result.allEntities.find(_.id == entity2.id).get

    resultEntity1.position shouldBe expectedPosition1
    resultEntity1.speed shouldBe Some(expectedSpeed1)
    resultEntity2.position shouldBe expectedPosition2
    resultEntity2.speed shouldBe Some(expectedSpeed2)

  test("the rule should use rotated rectangle extents at the scene border"):
    val entity = makeMovingEntityRectangle(
      id = "rotated",
      position = Vector2D(10.0, 50.0),
      width = 20.0,
      height = 10.0,
      speed = Vector2D(-1.0, 0.0)
    ).rotateTo(30.0).value.withWeight(1).value
    val scene = sceneWithEntities(List(entity))
    val expectedHalfWidth = SizeHelper.horizontalShapeSize(entity) / 2

    val result  = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value
    val updated = result.allEntities.find(_.id == entity.id).value

    updated.position.x shouldBe expectedHalfWidth +- 1e-9