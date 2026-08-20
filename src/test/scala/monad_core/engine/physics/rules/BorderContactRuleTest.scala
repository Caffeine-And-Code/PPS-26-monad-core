package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.EngineEvent.CollisionDetected
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.core.traits.State
import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeMovingEntityCircle}
import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.helper.{BorderContactHelper, MockDetectorHelper, MockSceneHelper}
import monad_core.engine.model.{BorderSide, Vector2D}
import monad_core.engine.physics.core.NegativeDeltaTime
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
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

  private def testSingleWall(borderSide: BorderSide) =
    val defaultValues = BorderContactHelper.generateSingleWallEntities(
      borderSide,
      UpperLeftBound,
      LowerRightBound
    )

    val (entity, wall, collision, expectedPosition, expectedSpeed) = defaultValues

    val scene = sceneWithEntities(List(entity))

    given CollisionDetector = detectorWithCollisions(
      Map((entity.id.value, wall.id.value) -> (collision.normalVector, collision.penetrationDepth))
    )

    val outcome = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value
    val result  = outcome.state

    val resultEntity = result.allEntities.find(_.id == entity.id).get

    resultEntity.position shouldBe expectedPosition
    resultEntity.speed shouldBe Some(expectedSpeed)
    outcome.events shouldBe Vector(
      CollisionDetected(entity.id, CollisionTarget.Border(borderSide), collision)
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

    val scene = sceneWithEntities(List(entity))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (entity.id.value, leftWall.id.value) -> (
          leftCollision.normalVector,
          leftCollision.penetrationDepth
        ),
        (entity.id.value, bottomWall.id.value) -> (
          bottomCollision.normalVector,
          bottomCollision.penetrationDepth
        )
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == entity.id).get

    resultEntity.position shouldBe expectedPosition
    resultEntity.speed shouldBe Some(expectedSpeed)

  test("the rule should return NegativeDeltaTime when delta time is negative"):

    val mockScene = sceneWithEntities(List.empty)

    val result = Rule.apply(mockScene, NegativeDt)(using summon[CollisionDetector])

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when there are no entities"):
    val scene = sceneWithEntities(List())

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    result shouldBe scene

  test("the rule should not update a fixed entity, even if it is outside the scene borders"):

    val fixedEntity = makeFixedEntityCircle(
      position = Vector2D(-10, -10)
    )

    val scene = sceneWithEntities(List(fixedEntity))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

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
          collision1.penetrationDepth
        ),
        (entity2.id.value, wall2.id.value) -> (collision2.normalVector, collision2.penetrationDepth)
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity1 = result.allEntities.find(_.id == entity1.id).get
    val resultEntity2 = result.allEntities.find(_.id == entity2.id).get

    resultEntity1.position shouldBe expectedPosition1
    resultEntity1.speed shouldBe Some(expectedSpeed1)
    resultEntity2.position shouldBe expectedPosition2
    resultEntity2.speed shouldBe Some(expectedSpeed2)
