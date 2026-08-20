package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeMovingEntityCircle}
import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.model.{Entity, LocatableId, Vector2D}
import monad_core.engine.physics.core.*
import monad_core.engine.helper.{MockDetectorHelper, MockSceneHelper}
import monad_core.engine.physics.utils.PhysicsUtil
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class KinematicsRuleTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with MockDetectorHelper
    with MockSceneHelper:

  private val Rule = KinematicsRule.kinematicsRule

  private val MockScene   = mock[State]
  given CollisionDetector = mock[CollisionDetector]

  test("the rule should return NegativeDeltaTime when delta time is negative"):

    val result = Rule.apply(MockScene, NegativeDt)(using summon[CollisionDetector])

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when the entities map is empty"):
    val scene = sceneWithEntities(List())

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector])

    result.value.state shouldBe scene

  test("the rule should not update the scene if the entity has no speed (fixed entity)"):
    val fixedEntity = makeFixedEntityCircle()

    val scene = sceneWithEntities(List(fixedEntity))

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector])

    val resultEntity = result.value.state.allEntities.find(_.id == fixedEntity.id).value

    resultEntity.position shouldBe fixedEntity.position

  test("the rule should move an entity with speed successfully and update the scene"):

    val movingEntity = makeMovingEntityCircle(
      id = "moving"
    )

    val scene = sceneWithEntities(List(movingEntity))

    val expectedPosition = PhysicsUtil
      .nextPosition(
        movingEntity.position,
        movingEntity.speed.value,
        DeltaTimeOneSecond,
        scene.bounds.upperLeft,
        scene.bounds.lowerRight
      )
      .value

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector])

    val resultEntity = result.value.state.allEntities.find(_.id == movingEntity.id).value

    resultEntity.position shouldBe expectedPosition

  test("the rule should move multiple entities with speed successfully and update the scene"):

    val entity1 = makeMovingEntityCircle(
      id = "entity1",
      position = Vector2D(0, 0),
      speed = Vector2D(1, 0)
    )

    val entity2 = makeMovingEntityCircle(
      id = "entity2",
      position = Vector2D(0, 0),
      speed = Vector2D(0, 1)
    )

    val scene = sceneWithEntities(List(entity1, entity2))

    val expectedPosition1 = PhysicsUtil
      .nextPosition(
        entity1.position,
        entity1.speed.value,
        DeltaTimeOneSecond,
        scene.bounds.upperLeft,
        scene.bounds.lowerRight
      )
      .value

    val expectedPosition2 = PhysicsUtil
      .nextPosition(
        entity2.position,
        entity2.speed.value,
        DeltaTimeOneSecond,
        scene.bounds.upperLeft,
        scene.bounds.lowerRight
      )
      .value

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector])

    val resultEntity1 = result.value.state.allEntities.find(_.id == entity1.id).value
    val resultEntity2 = result.value.state.allEntities.find(_.id == entity2.id).value

    resultEntity1.position shouldBe expectedPosition1
    resultEntity2.position shouldBe expectedPosition2

  test("moveEntity should report the entity id when speed is missing"):
    val entity = makeFixedEntityCircle(id = "fixed-without-speed")
    val scene  = sceneWithEntities(List(entity))

    KinematicsRule.moveEntity(scene, entity, DeltaTimeOneSecond) shouldBe Left(
      PhysicsRuleError(
        s"Entity ${entity.id} has no speed to apply kinematics"
      )
    )
