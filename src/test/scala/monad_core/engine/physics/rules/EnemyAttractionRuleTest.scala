package monad_core.engine.physics.rules

import engine.model.*
import monad_core.engine.model.Entity.*
import engine.physics.core.*
import monad_core.engine.model.{Entity, LocatableId, Team, Vector2D}
import monad_core.engine.physics.core.{NegativeDeltaTime, PhysicsRule, PhysicsState}
import monad_core.engine.physics.rules.EnemyAttractionRule.given
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnemyAttractionRuleTest extends AnyFunSuite with Matchers with MockFactory:

  trait TestScene
  trait TestDetector

  given TestDetector = mock[TestDetector]

  private val DeltaTimeOneSecond = 1_000_000_000L
  private val NegativeDt = -1L
  private val EntityRadius = 1.0
  private val InitialScene = mock[TestScene]

  private def makeEntity(id: String, position: Vector2D): Entity =
    Entity.circle(id = id, position = position, radius = EntityRadius).value

  private def makeMovingEntity(id: String, position: Vector2D, speed: Vector2D): Entity =
    makeEntity(id, position).withSpeed(speed).value

  private def addTeam(entity: Entity, teamId: String): Entity =
    entity.withTeamId(teamId).value

  private def makeTeam(id: String, enemies: Set[String] = Set.empty): Team =
    Team.create(id, enemies).value

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

    (mockState.getTeams(_: TestScene))
      .expects(InitialScene)
      .returning(Map.empty)
      .once()

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]

    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not update an entity when it has no enemy"):
    val entityId = "entity"
    val entityPositionX = 0.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val entitySpeedX = 1.0
    val entitySpeedY = 1.0
    val entitySpeed = Vector2D(entitySpeedX, entitySpeedY)
    val entityTeamId = "teamA"
    val otherTeamId = "teamB"

    val mockState = mock[PhysicsState[TestScene]]

    val entity = addTeam(
      makeMovingEntity(
        id = entityId,
        position = entityPosition,
        speed = entitySpeed
      ),
      entityTeamId
    )

    val entityTeam = makeTeam(
      id = entityTeamId,
      enemies = Set(otherTeamId)
    )

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity.id -> entity))
      .once()

    (mockState.getTeams(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entityTeam.id -> entityTeam))
      .once()

    mockState.updateEntity.expects(*, *, *).never()

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]

    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not update a fixed entity even when an enemy exists"):
    val fixedEntityId = "fixed-entity"
    val fixedEntityPositionX = 0.0
    val fixedEntityPositionY = 0.0
    val fixedEntityPosition = Vector2D(fixedEntityPositionX, fixedEntityPositionY)
    val fixedEntityTeamId = "teamA"
    val enemyId = "enemy"
    val enemyPositionX = 3.0
    val enemyPositionY = 4.0
    val enemyPosition = Vector2D(enemyPositionX, enemyPositionY)
    val enemyTeamId = "teamB"

    val mockState = mock[PhysicsState[TestScene]]

    val fixedEntity = addTeam(
      makeEntity(
        id = fixedEntityId,
        position = fixedEntityPosition
      ),
      fixedEntityTeamId
    )

    val enemy = addTeam(
      makeEntity(
        id = enemyId,
        position = enemyPosition
      ),
      enemyTeamId
    )

    val fixedEntityTeam = makeTeam(fixedEntityTeamId, Set(enemyTeamId))
    val enemyTeam = makeTeam(enemyTeamId)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(
        Map(
          fixedEntity.id -> fixedEntity,
          enemy.id -> enemy
        )
      )
      .once()

    (mockState.getTeams(_: TestScene))
      .expects(InitialScene)
      .returning(
        Map(
          fixedEntityTeam.id -> fixedEntityTeam,
          enemyTeam.id -> enemyTeam
        )
      )
      .once()

    mockState.updateEntity.expects(*, *, *).never()

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should accelerate a mobile entity toward its nearest enemy"):
    val entityId = "entity"
    val entityPositionX = 3.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val entitySpeedX = 0.0
    val entitySpeedY = 0.0
    val entitySpeed = Vector2D(entitySpeedX, entitySpeedY)
    val entityTeamId = "teamA"
    val enemyId = "enemy"
    val enemyPositionX = 3.0
    val enemyPositionY = 4.0
    val enemyPosition = Vector2D(enemyPositionX, enemyPositionY)
    val enemyTeamId = "teamB"
    val expectedSpeedX = 0.0
    val expectedSpeedY = 1.0
    val expectedSpeed = Vector2D(expectedSpeedX, expectedSpeedY)

    val finalScene = mock[TestScene]
    val mockState = mock[PhysicsState[TestScene]]

    val entity = addTeam(
      makeMovingEntity(
        id = entityId,
        position = entityPosition,
        speed = entitySpeed
      ),
      entityTeamId
    )

    val enemy = addTeam(
      makeEntity(
        id = enemyId,
        position = enemyPosition
      ),
      enemyTeamId
    )

    val entityTeam = makeTeam(entityTeamId, Set(enemyTeamId))
    val enemyTeam = makeTeam(enemyTeamId)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(
        Map(
          entity.id -> entity,
          enemy.id -> enemy
        )
      )
      .once()

    (mockState.getTeams(_: TestScene))
      .expects(InitialScene)
      .returning(
        Map(
          entityTeam.id -> entityTeam,
          enemyTeam.id -> enemyTeam
        )
      )
      .once()

    var caughtEntity: Option[Entity] = None

    (mockState.updateEntity(_: TestScene, _: LocatableId, _: Entity))
      .expects(InitialScene, entity.id, *)
      .onCall: (_, _, updatedEntity) =>
        caughtEntity = Some(updatedEntity)
        finalScene

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]

    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe finalScene
    caughtEntity.value.speed shouldBe Some(expectedSpeed)

  test("the rule should update every mobile entity that has an enemy"):
    val entity1Id = "entity1"
    val entity1PositionX = 0.0
    val entity1PositionY = 0.0
    val entity1Position = Vector2D(entity1PositionX, entity1PositionY)
    val entity1SpeedX = 0.0
    val entity1SpeedY = 0.0
    val entity1Speed = Vector2D(entity1SpeedX, entity1SpeedY)
    val entity2Id = "entity2"
    val entity2PositionX = 3.0
    val entity2PositionY = 0.0
    val entity2Position = Vector2D(entity2PositionX, entity2PositionY)
    val entity2SpeedX = 0.0
    val entity2SpeedY = 0.0
    val entity2Speed = Vector2D(entity2SpeedX, entity2SpeedY)
    val entityTeamId = "teamA"
    val enemyId = "enemy"
    val enemyPositionX = 3.0
    val enemyPositionY = 4.0
    val enemyPosition = Vector2D(enemyPositionX, enemyPositionY)
    val enemyTeamId = "teamB"
    val expectedSpeed1X = 0.6000000000000001
    val expectedSpeed1Y = 0.8
    val expectedSpeed1 = Vector2D(expectedSpeed1X, expectedSpeed1Y)
    val expectedSpeed2X = 0.0
    val expectedSpeed2Y = 1.0
    val expectedSpeed2 = Vector2D(expectedSpeed2X, expectedSpeed2Y)

    val afterFirstUpdate = mock[TestScene]
    val finalScene = mock[TestScene]
    val mockState = mock[PhysicsState[TestScene]]

    val entity1 = addTeam(
      makeMovingEntity(
        id = entity1Id,
        position = entity1Position,
        speed = entity1Speed
      ),
      entityTeamId
    )

    val entity2 = addTeam(
      makeMovingEntity(
        id = entity2Id,
        position = entity2Position,
        speed = entity2Speed
      ),
      entityTeamId
    )

    val enemy = addTeam(
      makeEntity(
        id = enemyId,
        position = enemyPosition
      ),
      enemyTeamId
    )

    val entityTeam = makeTeam(entityTeamId, Set(enemyTeamId))
    val enemyTeam = makeTeam(enemyTeamId)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(
        scala.collection.immutable.ListMap(
          entity1.id -> entity1,
          entity2.id -> entity2,
          enemy.id -> enemy
        )
      )
      .once()

    (mockState.getTeams(_: TestScene))
      .expects(InitialScene)
      .returning(
        Map(
          entityTeam.id -> entityTeam,
          enemyTeam.id -> enemyTeam
        )
      )
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