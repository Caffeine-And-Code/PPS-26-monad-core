package monad_core.engine.physics.rules

import engine.model.*
import monad_core.engine.model.Entity.*
import engine.physics.core.*
import monad_core.engine.model.{Entity, LocatableId, Vector2D}
import monad_core.engine.physics.core.{NegativeDeltaTime, PhysicsRule, PhysicsState}
import monad_core.engine.physics.rules.{Collision, CollisionResolutionDetection}
import monad_core.engine.physics.rules.CollisionResolutionRule.given
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollisionResolutionRuleTest extends AnyFunSuite with Matchers with MockFactory:

  trait TestScene
  trait TestDetector

  given TestDetector = mock[TestDetector]
  given CollisionResolutionDetection[TestDetector] = mock[CollisionResolutionDetection[TestDetector]]

  private val DeltaTimeOneSecond = 1_000_000_000L
  private val NegativeDt = -1L
  private val EntityRadius = 1.0
  private val InitialScene = mock[TestScene]

  private def makeEntity(id: String, position: Vector2D): Entity =
    Entity.circle(id = id, position = position, radius = EntityRadius).value

  private def makeMovingEntity(id: String, position: Vector2D, speed: Vector2D): Entity =
    makeEntity(id, position).withSpeed(speed).value

  private def makeCollision(normal: Vector2D, depth: Double = 1.0): Collision =
    new Collision:
      override val normalVector: Vector2D = normal
      override val penetrationDepth: Double = depth

  test("the rule should return NegativeDeltaTime when delta time is negative"):
    val mockState = mock[PhysicsState[TestScene]]
    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], NegativeDt)

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when there are no entities"):
    val mockState = mock[PhysicsState[TestScene]]

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map.empty)
      .once()

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not update an entity when it has no speed (fixed entity)"):
    val fixedEntityId = "fixed-entity"
    val fixedEntityPositionX = 0.0
    val fixedEntityPositionY = 0.0
    val fixedEntityPosition = Vector2D(fixedEntityPositionX, fixedEntityPositionY)

    val mockState = mock[PhysicsState[TestScene]]
    val fixedEntity = makeEntity(id = fixedEntityId, position = fixedEntityPosition)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(fixedEntity.id -> fixedEntity))
      .once()

    given PhysicsState[TestScene] = mockState

    mockState.updateEntity.expects(*, *, *).never()

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not update an entity when no collision is detected"):
    val entity1Id = "entity1"
    val entity1PositionX = 0.0
    val entity1PositionY = 0.0
    val entity1Position = Vector2D(entity1PositionX, entity1PositionY)
    val entity1SpeedX = 2.0
    val entity1SpeedY = 0.0
    val entity1Speed = Vector2D(entity1SpeedX, entity1SpeedY)

    val entity2Id = "other-entity"
    val entity2PositionX = 5.0
    val entity2PositionY = 0.0
    val entity2Position = Vector2D(entity2PositionX, entity2PositionY)

    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[CollisionResolutionDetection[TestDetector]]

    val entity1 = makeMovingEntity(id = entity1Id, position = entity1Position, speed = entity1Speed)
    val entity2 = makeEntity(id = entity2Id, position = entity2Position)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity1.id -> entity1, entity2.id -> entity2))
      .once()

    (mockDetection.collision(_: TestDetector, _: Entity, _: Entity))
      .expects(*, entity1, entity2)
      .returning(None)
      .once()

    given PhysicsState[TestScene] = mockState

    mockState.updateEntity.expects(*, *, *).never()

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not bounce an entity moving away from or parallel to the collision normal"):
    val entity1Id = "entity1"
    val entity1PositionX = 0.0
    val entity1PositionY = 0.0
    val entity1Position = Vector2D(entity1PositionX, entity1PositionY)
    val entity1SpeedX = 2.0
    val entity1SpeedY = 0.0
    val entity1Speed = Vector2D(entity1SpeedX, entity1SpeedY)

    val entity2Id = "entity2"
    val entity2PositionX = 0.0
    val entity2PositionY = 0.0
    val entity2Position = Vector2D(entity2PositionX, entity2PositionY)

    val normalX = 1.0
    val normalY = 0.0
    val collisionNormal = Vector2D(normalX, normalY)

    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[CollisionResolutionDetection[TestDetector]]

    val entity1 = makeMovingEntity(id = entity1Id, position = entity1Position, speed = entity1Speed)
    val entity2 = makeEntity(id = entity2Id, position = entity2Position)
    val collision = makeCollision(collisionNormal)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity1.id -> entity1, entity2.id -> entity2))
      .once()

    (mockDetection.collision(_: TestDetector, _: Entity, _: Entity))
      .expects(*, entity1, entity2)
      .returning(Some(collision))
      .once()

    given PhysicsState[TestScene] = mockState

    mockState.updateEntity.expects(*, *, *).never()

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should resolve collision and bounce a mobile entity colliding with a fixed entity"):
    val movingEntityId = "moving-entity"
    val movingEntityPositionX = 0.0
    val movingEntityPositionY = 0.0
    val movingEntityPosition = Vector2D(movingEntityPositionX, movingEntityPositionY)
    val movingEntitySpeedX = 2.0
    val movingEntitySpeedY = 0.0
    val movingEntitySpeed = Vector2D(movingEntitySpeedX, movingEntitySpeedY)

    val fixedEntityId = "fixed-entity"
    val fixedEntityPositionX = 2.0
    val fixedEntityPositionY = 0.0
    val fixedEntityPosition = Vector2D(fixedEntityPositionX, fixedEntityPositionY)

    val normalX = -1.0
    val normalY = 0.0
    val collisionNormal = Vector2D(normalX, normalY)

    val expectedSpeedX = -2.0
    val expectedSpeedY = 0.0
    val expectedSpeed = Vector2D(expectedSpeedX, expectedSpeedY)

    val finalScene = mock[TestScene]
    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[CollisionResolutionDetection[TestDetector]]

    val movingEntity = makeMovingEntity(movingEntityId, movingEntityPosition, movingEntitySpeed)
    val fixedEntity = makeEntity(fixedEntityId, fixedEntityPosition)
    val collision = makeCollision(collisionNormal)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(
        scala.collection.immutable.ListMap(
          movingEntity.id -> movingEntity,
          fixedEntity.id -> fixedEntity
        )
      )
      .once()

    (mockDetection.collision(_: TestDetector, _: Entity, _: Entity))
      .expects(*, movingEntity, fixedEntity)
      .returning(Some(collision))
      .once()

    var caughtEntity: Option[Entity] = None

    (mockState.updateEntity(_: TestScene, _: LocatableId, _: Entity))
      .expects(InitialScene, movingEntity.id, *)
      .onCall: (_, _, updatedEntity) =>
        caughtEntity = Some(updatedEntity)
        finalScene

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe finalScene
    caughtEntity.value.speed shouldBe Some(expectedSpeed)

  test("the rule should update multiple mobile entities when they collide with each other"):
    val entity1Id = "entity1"
    val entity1PositionX = 0.0
    val entity1PositionY = 0.0
    val entity1Position = Vector2D(entity1PositionX, entity1PositionY)
    val entity1SpeedX = 3.0
    val entity1SpeedY = 0.0
    val entity1Speed = Vector2D(entity1SpeedX, entity1SpeedY)

    val entity2Id = "entity2"
    val entity2PositionX = 4.0
    val entity2PositionY = 0.0
    val entity2Position = Vector2D(entity2PositionX, entity2PositionY)
    val entity2SpeedX = -2.0
    val entity2SpeedY = 0.0
    val entity2Speed = Vector2D(entity2SpeedX, entity2SpeedY)

    val normal1X = -1.0
    val normal1Y = 0.0
    val normalVector1 = Vector2D(normal1X, normal1Y)

    val normal2X = 1.0
    val normal2Y = 0.0
    val normalVector2 = Vector2D(normal2X, normal2Y)

    val expectedSpeed1X = -3.0
    val expectedSpeed1Y = 0.0
    val expectedSpeed1 = Vector2D(expectedSpeed1X, expectedSpeed1Y)

    val expectedSpeed2X = 2.0
    val expectedSpeed2Y = 0.0
    val expectedSpeed2 = Vector2D(expectedSpeed2X, expectedSpeed2Y)

    val afterFirstUpdate = mock[TestScene]
    val finalScene = mock[TestScene]
    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[CollisionResolutionDetection[TestDetector]]

    val entity1 = makeMovingEntity(entity1Id, entity1Position, entity1Speed)
    val entity2 = makeMovingEntity(entity2Id, entity2Position, entity2Speed)
    val collision1 = makeCollision(normalVector1)
    val collision2 = makeCollision(normalVector2)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(
        scala.collection.immutable.ListMap(
          entity1.id -> entity1,
          entity2.id -> entity2
        )
      )
      .once()

    (mockDetection.collision(_: TestDetector, _: Entity, _: Entity))
      .expects(*, entity1, entity2)
      .returning(Some(collision1))
      .once()

    (mockDetection.collision(_: TestDetector, _: Entity, _: Entity))
      .expects(*, entity2, entity1)
      .returning(Some(collision2))
      .once()

    var updatedEntities = Map.empty[LocatableId, Entity]

    inSequence:
      (mockState.updateEntity(_: TestScene, _: LocatableId, _: Entity))
        .expects(InitialScene, entity1.id, *)
        .onCall: (_, id, updatedEntity) =>
          updatedEntities += id -> updatedEntity
          afterFirstUpdate

      (mockState.updateEntity(_: TestScene, _: LocatableId, _: Entity))
        .expects(afterFirstUpdate, entity2.id, *)
        .onCall: (_, id, updatedEntity) =>
          updatedEntities += id -> updatedEntity
          finalScene

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe finalScene
    updatedEntities(entity1.id).speed shouldBe Some(expectedSpeed1)
    updatedEntities(entity2.id).speed shouldBe Some(expectedSpeed2)