package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.Event.EntityCollisionDetectedEvent
import monad_core.engine.core.traits.State
import monad_core.engine.geometry.Collision
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeMovingEntityCircle,
  makeMovingEntityRectangle
}
import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.model.*
import monad_core.engine.model.Entity.*
import monad_core.engine.physics.core.*
import monad_core.engine.helper.{MockDetectorHelper, MockSceneHelper}
import monad_core.engine.physics.utils.{CollisionResolver, PhysicsUtil}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollisionResolutionRuleTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with MockDetectorHelper
    with MockSceneHelper:

  private val Rule = CollisionResolutionRule.collisionResolutionRule

  private val MockScene   = sceneWithEntities(List())
  given CollisionDetector = mock[CollisionDetector]

  test("the rule should return NegativeDeltaTime when delta time is negative"):

    val result = Rule.apply(MockScene, NegativeDt)(using summon[CollisionDetector])

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when there are no entities"):

    val scene = sceneWithEntities(List())

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    result shouldBe scene

  test("the rule should not update an entity when no collision is detected"):

    val entity1 = makeMovingEntityCircle(id = "entity1")
    val entity2 = makeFixedEntityCircle(id = "entity2")

    val scene = sceneWithEntities(List(entity1, entity2))

    given CollisionDetector = detectorWithoutCollision()

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == entity1.id).value

    resultEntity.position shouldBe entity1.position
    resultEntity.speed shouldBe entity1.speed

  test("the rule should emit the collision detected during resolution without detecting it twice"):
    val entity1   = makeMovingEntityCircle(id = "entity1", position = Vector2D(10, 10))
    val entity2   = makeFixedEntityCircle(id = "entity2", position = Vector2D(11, 10))
    val collision = Collision(Vector2D(1, 0), 1)
    val scene     = sceneWithEntities(List(entity1, entity2))
    val detector  = mock[CollisionDetector]

    detector.collision.expects(entity1, entity2).returning(Some(collision)).once()

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using detector).value

    result.events shouldBe Vector(
      EntityCollisionDetectedEvent(entity1.id, entity2, collision)
    )

  test("the rule should push mobile entity outside collision with fixed entity"):
    val mobileEntity          = makeMovingEntityCircle(id = "mobile", position = Vector2D(0, 0))
    val fixedEntity           = makeFixedEntityCircle(id = "fixed", position = Vector2D(1, 0))
    val collisionNormal       = Vector2D(1, 0)
    val mobileCollisionNormal = collisionNormal.flip
    val collisionDepth        = 1.0

    val expectedPosition =
      PhysicsUtil.pushMobileOverlappingFixed(
        mobileEntity.position,
        mobileCollisionNormal,
        collisionDepth
      )
    val scene = sceneWithEntities(List(mobileEntity, fixedEntity))

    given CollisionDetector = detectorWithCollisions(
      Map((mobileEntity.id.value, fixedEntity.id.value) -> (collisionNormal, collisionDepth))
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    result.allEntities.find(_.id == mobileEntity.id).value.position shouldBe expectedPosition

  test(
    "the rule should resolve collision and bounce only a mobile entity colliding with a fixed entity"
  ):
    val collisionNormal       = Vector2D(-1, 0)
    val mobileCollisionNormal = collisionNormal.flip
    val collisionDepth        = 1.0

    val movingEntity = makeMovingEntityCircle(
      id = "moving",
      position = Vector2D(1.0, 1.0),
      speed = Vector2D(1.0, 0.0)
    )
    val fixedEntity = makeFixedEntityCircle(
      id = "fixed",
      position = Vector2D(5.0, 5.0)
    )

    val expectedMovingPosition = PhysicsUtil.pushMobileOverlappingFixed(
      movingEntity.position,
      mobileCollisionNormal,
      collisionDepth
    )
    val expectedMovingSpeed =
      PhysicsUtil.reflectOnFixed(movingEntity.speed.value, mobileCollisionNormal)

    val scene = sceneWithEntities(List(movingEntity, fixedEntity))

    given CollisionDetector = detectorWithCollisions(
      Map((movingEntity.id.value, fixedEntity.id.value) -> (collisionNormal, collisionDepth))
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultMoving = result.allEntities.find(_.id == movingEntity.id).value
    val resultFixed  = result.allEntities.find(_.id == fixedEntity.id).value

    resultMoving.position shouldBe expectedMovingPosition
    resultFixed.position shouldBe fixedEntity.position
    resultMoving.speed.value shouldBe expectedMovingSpeed
    resultFixed.speed shouldBe None

  test("the rule should update multiple mobile entities when they collide with each other"):

    val entity1 = makeMovingEntityCircle(
      id = "entity1",
      position = Vector2D(0, 0),
      speed = Vector2D(1, 0)
    ).withWeight(1).value

    val entity2 = makeMovingEntityCircle(
      id = "entity2",
      position = Vector2D(1, 0),
      speed = Vector2D(-1, 0)
    ).withWeight(2).value

    val collisionNormal   = Vector2D(1, 0)
    val firstEntityNormal = collisionNormal.flip
    val collisionDepth    = 1.0

    val expectedPosition1 = PhysicsUtil
      .pushMobileOverlappingMobile(
        entity1.position,
        firstEntityNormal,
        collisionDepth,
        entity1.weight,
        entity2.weight
      )
      .value

    val expectedPosition2 = PhysicsUtil
      .pushMobileOverlappingMobile(
        entity2.position,
        collisionNormal,
        collisionDepth,
        entity2.weight,
        entity1.weight
      )
      .value

    val expectedSpeed1 = PhysicsUtil
      .reflectOnMobile(
        entity1.speed.value,
        entity2.speed.value,
        firstEntityNormal,
        entity1.weight,
        entity2.weight
      )
      .value

    val expectedSpeed2 = PhysicsUtil
      .reflectOnMobile(
        entity2.speed.value,
        entity1.speed.value,
        collisionNormal,
        entity2.weight,
        entity1.weight
      )
      .value

    val scene = sceneWithEntities(List(entity1, entity2))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (entity1.id.value, entity2.id.value) -> (collisionNormal, collisionDepth)
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity1 = result.allEntities.find(_.id == entity1.id).value
    val resultEntity2 = result.allEntities.find(_.id == entity2.id).value

    resultEntity1.position shouldBe expectedPosition1
    resultEntity1.speed.value shouldBe expectedSpeed1
    resultEntity2.position shouldBe expectedPosition2
    resultEntity2.speed.value shouldBe expectedSpeed2

  test("the rule should return an error when a mobile entity has no weight"):

    val entity1 = makeMovingEntityCircle(
      id = "entity1",
      position = Vector2D(0, 0),
      speed = Vector2D(1, 0)
    ).withWeight(1).value

    val entity2 = makeMovingEntityCircle(
      id = "entity2",
      position = Vector2D(1, 0),
      speed = Vector2D(-1, 0)
    )

    val collisionNormal = Vector2D(1, 0)
    val collisionDepth  = 1.0

    val scene = sceneWithEntities(List(entity1, entity2))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (entity1.id.value, entity2.id.value) -> (collisionNormal, collisionDepth)
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector])

    result shouldBe Left(ZeroMassError())

  test("the rule should update equally circular and rectangular entities"):
    val initialPosition       = Vector2D(2, 2)
    val initialSpeed          = Vector2D(1, 0)
    val collisionNormal       = Vector2D(-1, 0)
    val mobileCollisionNormal = collisionNormal.flip
    val collisionDepth        = 1.0

    val circularEntity = makeMovingEntityCircle(
      id = "circular",
      position = initialPosition,
      speed = initialSpeed
    )

    val rectangularEntity = makeMovingEntityRectangle(
      id = "rectangular",
      position = initialPosition,
      speed = initialSpeed
    )

    val fixedEntity = makeFixedEntityCircle(
      id = "fixed",
      position = Vector2D(5.0, 5.0)
    )

    val expectedPosition = PhysicsUtil.pushMobileOverlappingFixed(
      initialPosition,
      mobileCollisionNormal,
      collisionDepth
    )

    val expectedSpeed = PhysicsUtil.reflectOnFixed(initialSpeed, mobileCollisionNormal)

    val scene = sceneWithEntities(List(circularEntity, rectangularEntity, fixedEntity))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (circularEntity.id.value, fixedEntity.id.value)    -> (collisionNormal, collisionDepth),
        (rectangularEntity.id.value, fixedEntity.id.value) -> (collisionNormal, collisionDepth)
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultCircular    = result.allEntities.find(_.id == circularEntity.id).value
    val resultRectangular = result.allEntities.find(_.id == rectangularEntity.id).value

    resultCircular.position shouldBe expectedPosition
    resultCircular.speed.value shouldBe expectedSpeed
    resultRectangular.position shouldBe expectedPosition
    resultRectangular.speed.value shouldBe expectedSpeed

  test("the rule should not apply a collision resolution when both entities are fixed"):
    val fixedEntity1 = makeFixedEntityCircle(
      id = "fixed1",
      position = Vector2D(0, 0)
    )

    val fixedEntity2 = makeFixedEntityCircle(
      id = "fixed2",
      position = Vector2D(1, 0)
    )

    val collisionNormal = Vector2D(-1, 0)
    val collisionDepth  = 1.0

    val scene = sceneWithEntities(List(fixedEntity1, fixedEntity2))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (fixedEntity1.id.value, fixedEntity2.id.value) -> (collisionNormal, collisionDepth)
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultFixed1 = result.allEntities.find(_.id == fixedEntity1.id).value
    val resultFixed2 = result.allEntities.find(_.id == fixedEntity2.id).value

    resultFixed1.position shouldBe fixedEntity1.position
    resultFixed1.speed shouldBe None
    resultFixed2.position shouldBe fixedEntity2.position
    resultFixed2.speed shouldBe None

  test("the rule should update an entity with multiple collisions"):
    val entity = makeMovingEntityCircle(
      id = "entity1",
      position = Vector2D(2, 1),
      speed = Vector2D(1, 0)
    )

    val wall1 = makeFixedEntityCircle(
      id = "wall1",
      position = Vector2D(1, 0)
    )

    val wall2 = makeFixedEntityCircle(
      id = "wall2",
      position = Vector2D(2, 0)
    )

    val collision1       = Collision(Vector2D(-1, 0), 1.0)
    val collision2       = Collision(Vector2D(0, -1), 5.0)
    val mobileCollision1 = collision1.copy(normalVector = collision1.normalVector.flip)
    val mobileCollision2 = collision2.copy(normalVector = collision2.normalVector.flip)

    val expectedEntity = CollisionResolver(
      Map(
        entity -> List(
          (wall1, mobileCollision1),
          (wall2, mobileCollision2)
        )
      )
    ).value.find(_.id == entity.id).value

    val scene = sceneWithEntities(List(entity, wall1, wall2))

    given CollisionDetector = detectorWithCollisions(
      Map(
        (entity.id.value, wall1.id.value) -> (collision1.normalVector, collision1.penetrationDepth),
        (entity.id.value, wall2.id.value) -> (collision2.normalVector, collision2.penetrationDepth)
      )
    )

    val result = Rule.apply(scene, DeltaTimeOneSecond)(using summon[CollisionDetector]).value.state

    val resultEntity = result.allEntities.find(_.id == entity.id).value

    resultEntity.position shouldBe expectedEntity.position
    resultEntity.speed shouldBe expectedEntity.speed
