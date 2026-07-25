package monad_core.engine.physics.rules

import monad_core.engine.model.Entity.*
import monad_core.engine.physics.core.*
import monad_core.engine.model.{Entity, LocatableId, Vector2D}
import monad_core.engine.physics.core.{NegativeDeltaTime, OutOfBoundEntity, PhysicsRule, PhysicsState}
import monad_core.engine.physics.rules.KinematicsRule.given
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class KinematicsRuleTest extends AnyFunSuite with Matchers with MockFactory:

  trait TestScene
  trait TestDetector

  given TestDetector = mock[TestDetector]

  private val DeltaTimeOneSecond = 1_000_000_000L
  private val EntityRadius = 1.0
  private val NegativeDt = -1L
  private val InitialScene = mock[TestScene]

  private def makeEntity(id: String, position: Vector2D): Entity =
    Entity.circle(id = id, position = position, radius = EntityRadius).value

  private def makeMovingEntity(id: String, position: Vector2D, speed: Vector2D): Entity =
    makeEntity(id, position).withSpeed(speed).value

  test("the rule should return NegativeDeltaTime when delta time is negative"):
    given PhysicsState[TestScene] = mock[PhysicsState[TestScene]]

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], NegativeDt)

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("the rule should return the unchanged scene when the entities map is empty"):
    val mockState = mock[PhysicsState[TestScene]]
    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map.empty)

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond)
    
    result.value shouldBe InitialScene

  test("the rule should not update the scene if the entity has no speed (fixed entity)"):
    val fixedEntityPositionX = 5.0
    val fixedEntityPositionY = 5.0
    val fixedEntityPosition = Vector2D(fixedEntityPositionX, fixedEntityPositionY)
    val fixedEntity = makeEntity("fixed", fixedEntityPosition)
    given TestDetector = mock[TestDetector]

    val mockState = mock[PhysicsState[TestScene]]
    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(fixedEntity.id -> fixedEntity))

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond)
    
    result.value shouldBe InitialScene

  test("the rule should move an entity with speed successfully and update the scene"):
    val movingEntityPositionX = 2.0
    val movingEntityPositionY = 3.0
    val movingEntityPosition = Vector2D(movingEntityPositionX, movingEntityPositionY)
    val movingEntitySpeedX = 4.0
    val movingEntitySpeedY = 5.0
    val movingEntitySpeed = Vector2D(movingEntitySpeedX, movingEntitySpeedY)
    val expectedPositionX = 6.0
    val expectedPositionY = 8.0
    val expectedPosition = Vector2D(expectedPositionX, expectedPositionY)

    val movingEntity = makeMovingEntity(
      "moving",
      movingEntityPosition,
      movingEntitySpeed
    )

    val finalScene = mock[TestScene]
    val mockState = mock[PhysicsState[TestScene]]

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(movingEntity.id -> movingEntity))

    var changedEntity: Option[Entity] = None

    (mockState.updateEntity(_: TestScene, _: LocatableId, _: Entity))
      .expects(InitialScene, movingEntity.id, *)
      .onCall { (_, _, updatedEntity) =>
        changedEntity = Some(updatedEntity)
        finalScene
      }

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond)

    result.value shouldBe finalScene
    changedEntity.value.position shouldBe expectedPosition

  test("the rule should propagate domain error when entity movement results in an invalid position"):
    val validPositionX = 0.0
    val validPositionY = 0.0
    val validPosition = Vector2D(validPositionX, validPositionY)
    val validSpeedX = -1.0
    val validSpeedY = 0.0
    val validSpeed = Vector2D(validSpeedX, validSpeedY)
    val invalidPositionX = -1.0
    val invalidPositionY = 0.0
    val invalidPosition = Vector2D(invalidPositionX, invalidPositionY)

    val invalidMoving = makeMovingEntity("invalid", validPosition, validSpeed)
    given TestDetector = mock[TestDetector]

    val mockState = mock[PhysicsState[TestScene]]
    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(invalidMoving.id -> invalidMoving))

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond)

    result shouldBe Left(OutOfBoundEntity(invalidPosition))

  test("the rule should move multiple entities with speed successfully and update the scene"):
    val entity1PositionX = 1.0
    val entity1PositionY = 1.0
    val entity1Position = Vector2D(entity1PositionX, entity1PositionY)
    val entity1SpeedX = 1.0
    val entity1SpeedY = 0.0
    val entity1Speed = Vector2D(entity1SpeedX, entity1SpeedY)
    val entity2PositionX = 2.0
    val entity2PositionY = 2.0
    val entity2Position = Vector2D(entity2PositionX, entity2PositionY)
    val entity2SpeedX = 0.0
    val entity2SpeedY = 1.0
    val entity2Speed = Vector2D(entity2SpeedX, entity2SpeedY)
    val expectedPosition1X = 2.0
    val expectedPosition1Y = 1.0
    val expectedPosition1 = Vector2D(expectedPosition1X, expectedPosition1Y)
    val expectedPosition2X = 2.0
    val expectedPosition2Y = 3.0
    val expectedPosition2 = Vector2D(expectedPosition2X, expectedPosition2Y)

    val entity1 = makeMovingEntity("entity1", entity1Position, entity1Speed)
    val entity2 = makeMovingEntity("entity2", entity2Position, entity2Speed)

    val finalScene = mock[TestScene]
    val mockState = mock[PhysicsState[TestScene]]

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity1.id -> entity1, entity2.id -> entity2))

    var changedEntities: Map[LocatableId, Entity] = Map.empty

    (mockState.updateEntity(_: TestScene, _: LocatableId, _: Entity))
      .expects(*, *, *)
      .anyNumberOfTimes()
      .onCall { (_, id, updatedEntity) =>
        changedEntities += (id -> updatedEntity)
        finalScene
      }

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond)

    result.value shouldBe finalScene
    changedEntities(entity1.id).position shouldBe expectedPosition1
    changedEntities(entity2.id).position shouldBe expectedPosition2