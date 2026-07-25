package monad_core.engine.physics.core

import monad_core.engine.model.{Entity, Team, Vector2D, Weight, WeightCannotBeNegativeOrZero}
import monad_core.engine.physics.core.{NegativeDeltaTime, PhysicsUtil}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsUtilTest extends AnyFunSuite with Matchers:

  private val NegativeDt = -1L

  private val EntityRadius = 1.0

  private def entityInTeam(
                            id: String,
                            position: Vector2D,
                            teamId: String
                          ): Entity =
    Entity
      .circle(id, position, EntityRadius)
      .value
      .withTeamId(teamId)
      .value

  private def team(
                    id: String,
                    enemies: Set[String] = Set.empty
                  ): Team =
    Team.create(id, enemies).value

  test("deltaSeconds should convert nanoseconds to seconds"):
    val nano = 1_500_000_000L
    val expectedSeconds = 1.5

    PhysicsUtil.deltaSeconds(nano).value shouldBe expectedSeconds

  test("deltaSeconds treats a negative delta time as zero"):

    PhysicsUtil.deltaSeconds(NegativeDt) shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("displacement should multiply speed by elapsed seconds"):
    val speedVectorX = 2.0
    val speedVectorY = 4.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val dt = 500_000_000L
    val expectedDisplacementX = 1.0
    val expectedDisplacementY = 2.0
    val expectedDisplacement = Vector2D(expectedDisplacementX, expectedDisplacementY)

    val result = PhysicsUtil.displacement(
      speed = speedVector,
      deltaTime = dt
    )

    result.value shouldBe expectedDisplacement

  test("displacement should return an error for negative delta time"):
    val speedVectorX = 1.0
    val speedVectorY = 1.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)

    val result = PhysicsUtil.displacement(
      speed = speedVector,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("nextPosition should apply Euler rule to compute the next position"):
    val positionVectorX = 5.0
    val positionVectorY = 2.0
    val positionVector = Vector2D(positionVectorX, positionVectorY)
    val speedVectorX = 4.0
    val speedVectorY = 6.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val dt = 500_000_000L
    val expectedNextPositionX = 7.0
    val expectedNextPositionY = 5.0
    val expectedNextPosition = Vector2D(expectedNextPositionX, expectedNextPositionY)

    val result = PhysicsUtil.nextPosition(
      position = positionVector,
      speed = speedVector,
      deltaTime = dt
    )

    result.value shouldBe expectedNextPosition

  test("nextPosition should return an error for negative delta time"):
    val positionVectorX = 5.0
    val positionVectorY = 2.0
    val positionVector = Vector2D(positionVectorX, positionVectorY)
    val speedVectorX = 4.0
    val speedVectorY = 6.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)

    val result = PhysicsUtil.nextPosition(
      position = positionVector,
      speed = speedVector,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("acceleration should be force divided by a (positive) mass"):
    val forceVectorX = 10.0
    val forceVectorY = 20.0
    val forceVector = Vector2D(forceVectorX, forceVectorY)
    val massValue = 2
    val expectedAccelerationX = 5.0
    val expectedAccelerationY = 10.0
    val expectedAcceleration = Vector2D(expectedAccelerationX, expectedAccelerationY)

    val result = PhysicsUtil.acceleration(
      force = forceVector,
      mass = Weight(massValue)
    )

    result.value shouldBe expectedAcceleration

  test("acceleration should return an error for zero or negative mass"):
    val forceVectorX = 10.0
    val forceVectorY = 20.0
    val forceVector = Vector2D(forceVectorX, forceVectorY)
    val zeroMassValue = 0
    val negativeMassValue = -2

    val resultZeroMass = PhysicsUtil.acceleration(
      force = forceVector,
      mass = Weight(zeroMassValue)
    )

    val resultNegativeMass = PhysicsUtil.acceleration(
      force = forceVector,
      mass = Weight(negativeMassValue)
    )

    resultZeroMass shouldBe Left(WeightCannotBeNegativeOrZero())
    resultNegativeMass shouldBe Left(WeightCannotBeNegativeOrZero())

  test("nextSpeed applies acceleration for the elapsed time"):
    val speedVectorX = 1.0
    val speedVectorY = 2.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val accelerationVectorX = 4.0
    val accelerationVectorY = 6.0
    val accelerationVector = Vector2D(accelerationVectorX, accelerationVectorY)
    val dt = 500_000_000L
    val expectedNextSpeedX = 3.0
    val expectedNextSpeedY = 5.0
    val expectedNextSpeed = Vector2D(expectedNextSpeedX, expectedNextSpeedY)

    val result = PhysicsUtil.nextSpeed(
      speed = speedVector,
      acceleration = accelerationVector,
      deltaTime = dt
    )

    result.value shouldBe expectedNextSpeed

  test("nextSpeed should return an error for negative delta time"):
    val speedVectorX = 1.0
    val speedVectorY = 2.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val accelerationVectorX = 4.0
    val accelerationVectorY = 6.0
    val accelerationVector = Vector2D(accelerationVectorX, accelerationVectorY)

    val result = PhysicsUtil.nextSpeed(
      speed = speedVector,
      acceleration = accelerationVector,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("friction should reduce speed proportionally to elapsed time"):
    val speedVectorX = 10.0
    val speedVectorY = 4.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val frictionIndex = 2.0
    val dt = 500_000_000L
    val expectedFrictionFactor = 1.0 - frictionIndex * (dt.toDouble / 1_000_000_000.0)
    val expectedFrictionSpeedX = speedVectorX * expectedFrictionFactor
    val expectedFrictionSpeedY = speedVectorY * expectedFrictionFactor
    val expectedFrictionSpeed = Vector2D(expectedFrictionSpeedX, expectedFrictionSpeedY)

    val result = PhysicsUtil.applyFriction(
      speed = speedVector,
      frictionIndex = frictionIndex,
      deltaTime = dt
    )

    result.value shouldBe expectedFrictionSpeed
    
  test("friction should not reverse speed"):
    val speedVectorX = 10.0
    val speedVectorY = 4.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val frictionIndex = 2.0
    val dt = 1_000_000_000L
    val expectedVectorX = 0.0
    val expectedVectorY = 0.0
    val expectedVector = Vector2D(expectedVectorX, expectedVectorY)

    val result = PhysicsUtil.applyFriction(
      speed = speedVector,
      frictionIndex = frictionIndex,
      deltaTime = dt
    )

    result.value shouldBe expectedVector

  test("friction should return an error for negative delta time"):
    val speedVectorX = 10.0
    val speedVectorY = 4.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val frictionIndex = 2.0

    val result = PhysicsUtil.applyFriction(
      speed = speedVector,
      frictionIndex = frictionIndex,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))
    
  test("direction returns None when the two positions coincide"):
    val fromToVectorX = 3.0
    val fromToVectorY = 4.0
    val fromToVector = Vector2D(fromToVectorX, fromToVectorY)
    
    val result = PhysicsUtil.direction(
      from = fromToVector,
      to = fromToVector
    )

    result shouldBe None
    
  test("direction returns the normalized vector toward the target"):
    val fromVectorX = 0.0
    val fromVectorY = 0.0
    val fromVector = Vector2D(fromVectorX, fromVectorY)
    val toVectorX = 3.0
    val toVectorY = 4.0
    val toVector = Vector2D(toVectorX, toVectorY)
    val expectedX = 0.6 +- 1e-12
    val expectedY = 0.8 +- 1e-12
    
    val result = PhysicsUtil.direction(
      from = fromVector,
      to = toVector
    )

    result.value.x shouldBe expectedX
    result.value.y shouldBe expectedY

  test("squaredDistance avoids an unnecessary square root"):
    val firstVectorX = 1.0
    val firstVectorY = 2.0
    val firstVector = Vector2D(firstVectorX, firstVectorY)
    val secondVectorX = 4.0
    val secondVectorY = 6.0
    val secondVector = Vector2D(secondVectorX, secondVectorY)
    val expectedValue = 25.0
    
    val result = PhysicsUtil.squaredDistance(
      first = firstVector,
      second = secondVector
    )

    result shouldBe expectedValue

  test("nearestEnemy should return None when there are no other entities"):
    val entityPositionX = 0.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val entityRadius = 1.0
    val entityId = "1"
    val entityTeamId = "teamA"
    val otherTeamId = "teamB"

    val entity = entityInTeam(entityId, entityPosition, entityTeamId)
    val entityTeam = team(entityTeamId, Set(otherTeamId))

    val entities = Map(entity.id -> entity)
    val teams = Map(entityTeam.id -> entityTeam)

    val result = PhysicsUtil.nearestEnemy(entity, entities, teams)

    result shouldBe None

  test("nearestEnemy should return None when the other entity is not an enemy"):
    val entity1PositionX = 0.0
    val entity1PositionY = 0.0
    val entity1Position = Vector2D(entity1PositionX, entity1PositionY)
    val entity1Radius = 1.0
    val entity1Id = "1"
    val entity1TeamId = "teamA"
    val entity2PositionX = 3.0
    val entity2PositionY = 4.0
    val entity2Position = Vector2D(entity2PositionX, entity2PositionY)
    val entity2Radius = 1.0
    val entity2Id = "2"
    val entity2TeamId = "teamA"

    val entity1 = entityInTeam(entity1Id, entity1Position, entity1TeamId)
    val entity2 = entityInTeam(entity2Id, entity2Position, entity2TeamId)

    val team1 = team(entity1TeamId)
    val team2 = team(entity2TeamId)

    val entities = Map(
      entity1.id -> entity1,
      entity2.id -> entity2
    )

    val teams = Map(
      team1.id -> team1,
      team2.id -> team2
    )

    val result = PhysicsUtil.nearestEnemy(entity1, entities, teams)

    result shouldBe None

  test("nearestEnemy should return the enemy when a single enemy entity is present"):
    val entity1PositionX = 0.0
    val entity1PositionY = 0.0
    val entity1Position = Vector2D(entity1PositionX, entity1PositionY)
    val entity1Radius = 1.0
    val entity1Id = "1"
    val entity1Team = "teamA"
    val entity2PositionX = 3.0
    val entity2PositionY = 4.0
    val entity2Position = Vector2D(entity2PositionX, entity2PositionY)
    val entity2Radius = 1.0
    val entity2Id = "2"
    val entity2Team = "teamB"

    val entity1 = entityInTeam(entity1Id, entity1Position, entity1Team)
    val entity2 = entityInTeam(entity2Id, entity2Position, entity2Team)

    val teamA = team(entity1Team, Set(entity2Team))
    val teamB = team(entity2Team, Set(entity1Team))

    val entities = Map(
      entity1.id -> entity1,
      entity2.id -> entity2
    )

    val teams = Map(
      teamA.id -> teamA,
      teamB.id -> teamB
    )

    val result = PhysicsUtil.nearestEnemy(entity1, entities, teams)

    result.value shouldBe entity2

  test("nearestEnemy should select the closest enemy when multiple enemies are present"):
    val entityPositionX = 0.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val entityRadius = 1.0
    val entityId = "1"
    val entityTeamId = "teamA"
    val farEnemyPositionX = 3.0
    val farEnemyPositionY = 4.0
    val farEnemyPosition = Vector2D(farEnemyPositionX, farEnemyPositionY)
    val farEnemyRadius = 1.0
    val farEnemyId = "2"
    val closeEnemyPositionX = 1.0
    val closeEnemyPositionY = 1.0
    val closeEnemyPosition = Vector2D(closeEnemyPositionX, closeEnemyPositionY)
    val closeEnemyRadius = 1.0
    val closeEnemyId = "3"
    val enemyTeamId = "teamB"

    val entity = entityInTeam(entityId, entityPosition, entityTeamId)
    val farEnemy = entityInTeam(farEnemyId, farEnemyPosition, enemyTeamId)
    val closeEnemy = entityInTeam(closeEnemyId, closeEnemyPosition, enemyTeamId)

    val teamA = team(entityTeamId, Set(enemyTeamId))
    val teamB = team(enemyTeamId, Set(entityTeamId))

    val entities = Map(
      entity.id -> entity,
      farEnemy.id -> farEnemy,
      closeEnemy.id -> closeEnemy
    )

    val teams = Map(
      teamA.id -> teamA,
      teamB.id -> teamB
    )

    val result = PhysicsUtil.nearestEnemy(entity, entities, teams)

    result.value shouldBe closeEnemy
