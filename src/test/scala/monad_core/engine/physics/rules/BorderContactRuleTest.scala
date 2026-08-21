package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.EngineEvent.CollisionDetected
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.core.traits.State
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeMovingEntityCircle,
  makeMovingEntityRectangle
}
import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.helper.{BorderContactHelper, MockDetectorHelper, MockStateHelper}
import monad_core.engine.model.{BorderSide, Vector2D}
import monad_core.engine.physics.core.NegativeDeltaTime
import monad_core.engine.physics.pathfinding.SizeHelper
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BorderContactRuleTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with MockStateHelper
    with MockDetectorHelper:

  private val Rule = BorderContactRule.borderContactRule

  private val UpperLeftBound  = Vector2D(0.0, 0.0)
  private val LowerRightBound = Vector2D(100.0, 100.0)

  given CollisionDetector = mock[CollisionDetector]

  private def testSingleWall(borderSide: BorderSide) =
    val defaultValues = BorderContactHelper.generateSingleWallEntities(
      borderSide,
      UpperLeftBound,
      LowerRightBound
    )

    val scene = stateWithEntities(List(defaultValues.entity))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (defaultValues.entity.id.value, defaultValues.wall.id.value) -> (
          defaultValues.collision.normalVector,
          defaultValues.collision.penetrationDepth,
          defaultValues.collision.collisionPoint
        )
      )
    )

    val outcome = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value
    val result  = outcome.state

    val resultEntity = result.allEntities.find(_.id == defaultValues.entity.id).get

    resultEntity.position shouldBe defaultValues.expectedPosition
    resultEntity.speed shouldBe Some(defaultValues.expectedSpeed)
    outcome.events shouldBe Vector(
      CollisionDetected(
        defaultValues.entity.id,
        CollisionTarget.Border(borderSide),
        defaultValues.collision
      )
    )

  private def testCornerWall(borderSideV: BorderSide, borderSideH: BorderSide) =
    val data = BorderContactHelper.generateCornerEntities(
      borderSideV,
      borderSideH,
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

    val scene = stateWithEntities(List(entity))

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

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == entity.id).get

    resultEntity.position shouldBe expectedPosition
    resultEntity.speed shouldBe Some(expectedSpeed)

  test("the rule should return NegativeDeltaTime when delta time is negative"):

    val mockScene = stateWithEntities(List.empty)

    val result = Rule.apply(mockScene, NegativeDt)(using summon[CollisionDetector])

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when there are no entities"):
    val scene = stateWithEntities(List())

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    result shouldBe scene

  test("the rule should not update a fixed entity, even if it is outside the scene borders"):

    val fixedEntity = makeFixedEntityCircle(
      position = Vector2D(-10, -10)
    )

    val scene = stateWithEntities(List(fixedEntity))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == fixedEntity.id).get
    resultEntity.position shouldBe fixedEntity.position
    resultEntity.speed shouldBe fixedEntity.speed

  test("the rule should not update a moving entity when it is within the scene borders"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(50, 50),
      speed = Vector2D(1, 1)
    )

    val scene               = stateWithEntities(List(entity))
    given CollisionDetector = detectorWithoutCollision()
    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == entity.id).get
    resultEntity.position shouldBe entity.position
    resultEntity.speed shouldBe entity.speed

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on left"
  ):
    testSingleWall(BorderSide.Left)

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on right"
  ):
    testSingleWall(BorderSide.Right)

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on top"
  ):
    testSingleWall(BorderSide.Top)

  test(
    "the rule should push and bounce a moving entity back inside the scene borders when it is outside on bottom"
  ):
    testSingleWall(BorderSide.Bottom)

  test("the rule should push and bounce a moving entity when it is outside on left and top sides"):
    testCornerWall(BorderSide.Left, BorderSide.Top)

  test("the rule should push and bounce a moving entity when it is outside on right and top sides"):
    testCornerWall(BorderSide.Right, BorderSide.Top)

  test(
    "the rule should push and bounce a moving entity when it is outside on right and bottom sides"
  ):
    testCornerWall(BorderSide.Right, BorderSide.Bottom)

  test(
    "the rule should push and bounce a moving entity when it is outside on left and bottom sides"
  ):
    testCornerWall(BorderSide.Left, BorderSide.Bottom)

  test("the rule should update multiple entities when they are outside the scene borders"):

    val data1 = BorderContactHelper.generateSingleWallEntities(
      BorderSide.Left,
      UpperLeftBound,
      LowerRightBound,
      entityId = "entity1"
    )

    val data2 = BorderContactHelper.generateSingleWallEntities(
      BorderSide.Top,
      UpperLeftBound,
      LowerRightBound,
      entityId = "entity2"
    )

    val entity1    = data1.entity
    val wall1      = data1.wall
    val collision1 = data1.collision

    val entity2    = data2.entity
    val wall2      = data2.wall
    val collision2 = data2.collision

    val expectedPosition1 = data1.expectedPosition
    val expectedSpeed1    = data1.expectedSpeed
    val expectedPosition2 = data2.expectedPosition
    val expectedSpeed2    = data2.expectedSpeed

    val scene = stateWithEntities(List(entity1, entity2))

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

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

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
    val scene             = stateWithEntities(List(entity))
    val expectedHalfWidth = SizeHelper.horizontalShapeSize(entity) / 2

    val result  = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state
    val updated = result.allEntities.find(_.id == entity.id).value

    updated.position.x shouldBe expectedHalfWidth +- 1e-9
