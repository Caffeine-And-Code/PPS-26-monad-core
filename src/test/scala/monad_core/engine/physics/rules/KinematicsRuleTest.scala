package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeMovingEntityCircle,
  makeMovingEntityRectangle
}
import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.helper.{MockDetectorHelper, MockStateHelper}
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.physics.core.*
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
    with MockStateHelper:

  private val Rule = KinematicsRule.kinematicsRule

  given CollisionDetector = mock[CollisionDetector]

  test("the rule should return NegativeDeltaTime when delta time is negative"):
    val entity = makeMovingEntityCircle()

    val state = stateWithEntities(List(entity))

    val result = Rule.apply(PhysicsContext(state, NegativeDt))

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged state when the entities map is empty"):
    val state = stateWithEntities(List())

    val result = Rule.apply(PhysicsContext(state, DeltaTimeOneSecond))

    result.value.state shouldBe state

  test("the rule should not update the state if the entity has no speed (fixed entity)"):
    val fixedEntity = makeFixedEntityCircle()

    val state = stateWithEntities(List(fixedEntity))

    val result = Rule.apply(PhysicsContext(state, DeltaTimeOneSecond))

    val resultEntity = result.value.state.allEntities.find(_.id == fixedEntity.id).value

    resultEntity.position shouldBe fixedEntity.position

  test("the rule should move an entity with speed successfully and update the state"):

    val movingEntity = makeMovingEntityCircle(
      id = "moving"
    )

    val state = stateWithEntities(List(movingEntity))

    val expectedPosition = PhysicsUtil
      .nextPosition(movingEntity.position, movingEntity.speed.value, DeltaTimeOneSecond)
      .value

    val result = Rule.apply(PhysicsContext(state, DeltaTimeOneSecond))

    val resultEntity = result.value.state.allEntities.find(_.id == movingEntity.id).value

    resultEntity.position shouldBe expectedPosition

  test("the rule should move multiple entities with speed successfully and update the state"):

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

    val state = stateWithEntities(List(entity1, entity2))

    val expectedPosition1 = PhysicsUtil
      .nextPosition(entity1.position, entity1.speed.value, DeltaTimeOneSecond)
      .value

    val expectedPosition2 = PhysicsUtil
      .nextPosition(entity2.position, entity2.speed.value, DeltaTimeOneSecond)
      .value

    val result = Rule.apply(PhysicsContext(state, DeltaTimeOneSecond))

    val resultEntity1 = result.value.state.allEntities.find(_.id == entity1.id).value
    val resultEntity2 = result.value.state.allEntities.find(_.id == entity2.id).value

    resultEntity1.position shouldBe expectedPosition1
    resultEntity2.position shouldBe expectedPosition2

  test("the rule should rotate an entity using angular speed"):
    val rotatingEntity = makeFixedEntityCircle(id = "rotating")
      .withAngularSpeed(Some(90.0))
    val state = stateWithEntities(List(rotatingEntity))

    val result = Rule.apply(PhysicsContext(state, DeltaTimeOneSecond)).value.state

    result.allEntities.find(_.id == rotatingEntity.id).value.rotation shouldBe 90.0

  test("angular integration should wrap rotations into a full turn"):

    val rotatingEntity = makeMovingEntityRectangle(
      id = "rotating",
      position = Vector2D(0.0, 0.0),
      width = 10.0,
      height = 5.0,
      rotation = 350.0
    ).withAngularSpeed(Some(20.0))

    val state = stateWithEntities(List(rotatingEntity))

    val result = Rule.apply(PhysicsContext(state, DeltaTimeOneSecond)).value.state

    result.allEntities.find(_.id == rotatingEntity.id).value.rotation shouldBe 10.0
