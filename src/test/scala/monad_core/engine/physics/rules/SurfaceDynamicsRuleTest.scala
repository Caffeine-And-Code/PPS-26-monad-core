package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.model.Entity.*
import monad_core.engine.physics.core.*
import monad_core.engine.physics.helper.PhysicsConstantHelper.*
import monad_core.engine.physics.helper.PhysicsEntityHelper.*
import monad_core.engine.physics.helper.PhysicsSurfaceHelper.*
import monad_core.engine.physics.helper.{PhysicsDetectorHelper, PhysicsSceneHelper}
import monad_core.engine.physics.rules.SurfaceDynamicsRule
import monad_core.engine.physics.utils.PhysicsUtil
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SurfaceDynamicsRuleTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with PhysicsDetectorHelper
    with PhysicsSceneHelper:

  private val Rule = SurfaceDynamicsRule.surfaceDynamicsRule

  private val MockScene   = mock[State]
  given CollisionDetector = mock[CollisionDetector]

  test("the rule should return NegativeDeltaTime when delta time is negative"):

    val result = Rule.apply(MockScene, NegativeDt)(using summon[CollisionDetector])

    result.shouldBe(Left(NegativeDeltaTime(NegativeDt)))

  test("the rule should return the unchanged scene when there are no entities"):
    val scene = sceneWithSurfaces(List(), List())

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    result shouldBe scene

  test("the rule should not update an entity when it is fixed"):

    val fixedEntity = makeFixedEntityCircle(
      id = "fixed"
    )

    val surface = makeSurfaceCircle(
      position = Vector2D(0, 0),
      radius = 5.0
    )
      .withAppliedForce(Vector2D(10, 0))
      .value
      .withFrictionIndex(0.1)
      .value

    val scene = sceneWithSurfaces(List(fixedEntity), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map((fixedEntity.id.value, surface.id.value) -> true)
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == fixedEntity.id).value

    resultEntity.speed shouldBe fixedEntity.speed

  test("the rule should not update an entity when it is outside the surface"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, 0),
      speed = Vector2D(1, 1)
    )

    val surface = makeSurfaceCircle(
      position = Vector2D(10, 10),
      radius = 5.0
    )
      .withAppliedForce(Vector2D(10, 0))
      .value
      .withFrictionIndex(0.1)
      .value

    val scene = sceneWithSurfaces(List(entity), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map((entity.id.value, surface.id.value) -> false)
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.speed shouldBe entity.speed

  test("the rule should not update an entity when surface has no force and no friction"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, 0),
      speed = Vector2D(1, 1)
    )

    val surface = makeSurfaceCircle(
      position = Vector2D(0, 0),
      radius = 5.0
    )

    val scene = sceneWithSurfaces(List(entity), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map((entity.id.value, surface.id.value) -> true)
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.speed shouldBe entity.speed

  test("the rule should apply only force when surface has force but no friction"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, 0),
      speed = Vector2D(1, 1)
    ).withWeight(1).value

    val surface = makeSurfaceCircle(
      position = Vector2D(0, 0),
      radius = 5.0
    )
      .withAppliedForce(Vector2D(10, 0))
      .value

    val acceleration = PhysicsUtil.acceleration(surface.appliedForce.value, entity.weight).value

    val expectedSpeedAfterForce = entity.speed.value + acceleration

    val scene = sceneWithSurfaces(List(entity), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map((entity.id.value, surface.id.value) -> true)
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.speed.value shouldBe expectedSpeedAfterForce

  test("the rule should apply only friction when surface has friction but no force"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, 0),
      speed = Vector2D(1, 1)
    ).withWeight(1).value

    val surface = makeSurfaceCircle(
      position = Vector2D(0, 0),
      radius = 5.0
    )
      .withFrictionIndex(0.1)
      .value

    val expectedSpeedAfterFriction = PhysicsUtil
      .applyFriction(
        entity.speed.value,
        surface.frictionIndex.value,
        DeltaTimeOneSecond
      )
      .value

    val scene = sceneWithSurfaces(List(entity), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map((entity.id.value, surface.id.value) -> true)
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.speed.value shouldBe expectedSpeedAfterFriction

  test("the rule should apply force and friction when entity is inside the surface"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, 0),
      speed = Vector2D(1, 1)
    ).withWeight(1).value

    val surface = makeSurfaceCircle(
      position = Vector2D(0, 0),
      radius = 5.0
    )
      .withAppliedForce(Vector2D(10, 0))
      .value
      .withFrictionIndex(0.1)
      .value

    val acceleration = PhysicsUtil.acceleration(surface.appliedForce.value, entity.weight).value

    val expectedSpeedAfterForce = entity.speed.value + acceleration

    val expectedSpeedAfterFriction = PhysicsUtil
      .applyFriction(
        expectedSpeedAfterForce,
        surface.frictionIndex.value,
        DeltaTimeOneSecond
      )
      .value

    val scene = sceneWithSurfaces(List(entity), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map((entity.id.value, surface.id.value) -> true)
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.speed.value shouldBe expectedSpeedAfterFriction

  test(
    "the rule should apply force and friction to multiple entities when they are inside the surface"
  ):

    val entity1 = makeMovingEntityCircle(
      id = "entity1",
      position = Vector2D(0, 0),
      speed = Vector2D(1, 1)
    ).withWeight(1).value

    val entity2 = makeMovingEntityCircle(
      id = "entity2",
      position = Vector2D(1, 1),
      speed = Vector2D(2, 2)
    ).withWeight(2).value

    val surface = makeSurfaceCircle(
      position = Vector2D(0, 0),
      radius = 5.0
    )
      .withAppliedForce(Vector2D(10, 0))
      .value
      .withFrictionIndex(0.1)
      .value

    val scene = sceneWithSurfaces(List(entity1, entity2), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map(
        (entity1.id.value, surface.id.value) -> true,
        (entity2.id.value, surface.id.value) -> true
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    val resultEntity1 = result.allEntities.find(_.id == entity1.id).value
    val resultEntity2 = result.allEntities.find(_.id == entity2.id).value

    val expectedSpeedAfterForceEntity1 =
      entity1.speed.value + PhysicsUtil
        .acceleration(surface.appliedForce.value, entity1.weight)
        .value
    val expectedSpeedAfterFrictionEntity1 =
      PhysicsUtil
        .applyFriction(
          expectedSpeedAfterForceEntity1,
          surface.frictionIndex.value,
          DeltaTimeOneSecond
        )
        .value

    val expectedSpeedAfterForceEntity2 =
      entity2.speed.value + PhysicsUtil
        .acceleration(surface.appliedForce.value, entity2.weight)
        .value
    val expectedSpeedAfterFrictionEntity2 =
      PhysicsUtil
        .applyFriction(
          expectedSpeedAfterForceEntity2,
          surface.frictionIndex.value,
          DeltaTimeOneSecond
        )
        .value

    resultEntity1.speed.value shouldBe expectedSpeedAfterFrictionEntity1
    resultEntity2.speed.value shouldBe expectedSpeedAfterFrictionEntity2

  test("the rule should return an error when trying to apply force to an entity with zero weight"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, 0),
      speed = Vector2D(1, 1)
    )

    val surface = makeSurfaceCircle(
      position = Vector2D(0, 0),
      radius = 5.0
    )
      .withAppliedForce(Vector2D(10, 0))
      .value

    val scene = sceneWithSurfaces(List(entity), List(surface))

    given CollisionDetector = detectorWithContaining(
      contains = Map((entity.id.value, surface.id.value) -> true)
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector])

    result shouldBe Left(ZeroMassError())
