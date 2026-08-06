package monad_core.engine.physics.utils

import monad_core.engine.model.*
import monad_core.engine.physics.core.{NegativeDeltaTime, ZeroMassError}
import monad_core.engine.physics.helper.PhysicsConstantHelper.*
import monad_core.engine.physics.helper.PhysicsEntityHelper.*
import monad_core.engine.physics.helper.PhysicsTeamHelper.*
import monad_core.engine.physics.utils.PhysicsUtil
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsUtilTest extends AnyFunSuite with Matchers:

  private val UpperLeftCorner = Vector2D(0.0, 0.0)
  private val LowerRightCorner = Vector2D(10.0, 10.0)
  
  test("deltaSeconds should convert nanoseconds to seconds"):
    val nano = 1_500_000_000L
    val expectedSeconds = 1.5
    
    PhysicsUtil.deltaSeconds(nano).value shouldBe expectedSeconds

  test("deltaSeconds treats a negative delta time as zero"):

    PhysicsUtil.deltaSeconds(NegativeDt) shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("displacement should multiply speed by elapsed seconds"):
    val speed = Vector2D(2.0, 4.0)
    val dt = 500_000_000L
    val dtS = PhysicsUtil.deltaSeconds(dt).value
    val expectedDisplacement = Vector2D(speed.x * dtS, speed.y * dtS)

    val result = PhysicsUtil.displacement(
      speed = speed,
      deltaTime = dt
    )

    result.value shouldBe expectedDisplacement

  test("displacement should return an error for negative delta time"):
    val speed = Vector2D(1.0, 1.0)

    val result = PhysicsUtil.displacement(
      speed = speed,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("nextPosition should apply Euler rule to compute the next position"):
    val position = Vector2D(5.0, 2.0)
    val speed = Vector2D(4.0, 6.0)
    val dt = 500_000_000L
    val dtS = PhysicsUtil.deltaSeconds(dt).value
    val expectedNextPosition = Vector2D(position.x + speed.x * dtS, position.y + speed.y * dtS)

    val result = PhysicsUtil.nextPosition(
      position = position,
      speed = speed,
      deltaTime = dt,
      upperLeftCorner = UpperLeftCorner,
      lowerRightCorner = LowerRightCorner
    )

    result.value shouldBe expectedNextPosition

  test("nextPosition should return an error for negative delta time"):
    val position = Vector2D(5.0, 2.0)
    val speed = Vector2D(4.0, 6.0)

    val result = PhysicsUtil.nextPosition(
      position = position,
      speed = speed,
      deltaTime = NegativeDt,
      upperLeftCorner = UpperLeftCorner,
      lowerRightCorner = LowerRightCorner
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("acceleration should be force divided by a (positive) mass"):
    val force = Vector2D(10.0, 20.0)
    val massValue = 2
    val expectedAcceleration = Vector2D(force.x / massValue.toDouble, force.y / massValue.toDouble)

    val result = PhysicsUtil.acceleration(
      force = force,
      mass = Some(Weight(massValue).value)
    )

    Console.println(s"Expected: $expectedAcceleration, Result: ${result.value}")
    
    result.value shouldBe expectedAcceleration

  test("acceleration should return an error for missing mass"):
    val force = Vector2D(10.0, 20.0)
    val zeroMassValue = 0
    val negativeMassValue = -2
    
    val resultMissingMass = PhysicsUtil.acceleration(
      force = force,
      mass = None
    )
    
    resultMissingMass shouldBe Left(ZeroMassError())

  test("nextSpeed applies acceleration for the elapsed time"):
    val speed = Vector2D(1.0, 2.0)
    val acceleration = Vector2D(4.0, 6.0)
    val dt = 500_000_000L
    val dtS = PhysicsUtil.deltaSeconds(dt).value
    val expectedNextSpeed = Vector2D(speed.x + acceleration.x * dtS, speed.y + acceleration.y * dtS)

    val result = PhysicsUtil.nextSpeed(
      speed = speed,
      acceleration = acceleration,
      deltaTime = dt
    )

    result.value shouldBe expectedNextSpeed

  test("nextSpeed should return an error for negative delta time"):
    val speed = Vector2D(1.0, 2.0)
    val acceleration = Vector2D(4.0, 6.0)

    val result = PhysicsUtil.nextSpeed(
      speed = speed,
      acceleration = acceleration,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("friction should reduce speed proportionally to elapsed time"):
    val speed = Vector2D(10.0, 4.0)
    val frictionIndex = 2.0
    val dt = 500_000_000L
    val expectedFrictionFactor = 1.0 - frictionIndex * (dt.toDouble / 1_000_000_000.0)
    val expectedFrictionSpeed = Vector2D(speed.x * expectedFrictionFactor, speed.y * expectedFrictionFactor)

    val result = PhysicsUtil.applyFriction(
      speed = speed,
      frictionIndex = frictionIndex,
      deltaTime = dt
    )

    result.value shouldBe expectedFrictionSpeed

  test("friction should not reverse speed"):
    val speed = Vector2D(10.0, 4.0)
    val frictionIndex = 2.0
    val dt = 1_000_000_000L
    val expectedVector = Vector2D(0.0, 0.0)

    val result = PhysicsUtil.applyFriction(
      speed = speed,
      frictionIndex = frictionIndex,
      deltaTime = dt
    )

    result.value shouldBe expectedVector

  test("friction should return an error for negative delta time"):
    val speed = Vector2D(10.0, 4.0)
    val frictionIndex = 2.0

    val result = PhysicsUtil.applyFriction(
      speed = speed,
      frictionIndex = frictionIndex,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))
    
  test("squaredDistance avoids an unnecessary square root"):
    val firstVectorX = 1.0
    val firstVectorY = 2.0
    val firstVector = Vector2D(firstVectorX, firstVectorY)
    val secondVectorX = 4.0
    val secondVectorY = 6.0
    val secondVector = Vector2D(secondVectorX, secondVectorY)
    val expectedValue = math.pow((secondVectorX - firstVectorX), 2) + math.pow((secondVectorY - firstVectorY), 2)

    val result = PhysicsUtil.squaredDistance(
      first = firstVector,
      second = secondVector
    )

    result shouldBe expectedValue
  
  test("direction returns None when the two positions coincide"):
    val fromToVector = Vector2D(3.0, 4.0)

    val result = PhysicsUtil.direction(
      from = fromToVector,
      to = fromToVector
    )

    result shouldBe None
  
  test("direction returns the normalized vector toward the target"):
    val fromVector = Vector2D(0.0, 0.0)
    val toVector = Vector2D(3.0, 4.0)

    val delta = Vector2D(toVector.x - fromVector.x, toVector.y - fromVector.y)
    val squaredLength = PhysicsUtil.squaredDistance(fromVector, toVector)
    val expected = delta * (1.0 / math.sqrt(squaredLength))

    val result = PhysicsUtil.direction(
      from = fromVector,
      to = toVector
    )

    result.value shouldBe expected

  test("nearestEnemy should return None when there are no other entities"):
    
    val entity = addTeam(
      makeMovingEntityCircle(
        id = "entity1"
      ),
      "teamA"
    )
    
    val entityTeam = makeTeam(entity.teamId.value.value, Set("teamB"))

    val result = PhysicsUtil.nearestEnemy(entity, List(entity), List(entityTeam))

    result shouldBe None

  test("nearestEnemy should return None when the other entity is not an enemy"):
    val entity1 = addTeam(
      makeMovingEntityCircle(
        id = "entity1",
        position = Vector2D(0.0, 0.0)
      ),
      "teamA"
    )
    
    val entity2 = addTeam(
      makeMovingEntityCircle(
        id = "entity2",
        position = Vector2D(3.0, 4.0)
      ),
      "teamA"
    )

    val team1 = makeTeam(entity1.teamId.value.value, Set("teamB"))

    val result = PhysicsUtil.nearestEnemy(entity1, List(entity1, entity2), List(team1))

    result shouldBe None

  test("nearestEnemy should return the enemy when a single enemy entity is present"):
    
    val entity1 = addTeam(
      makeMovingEntityCircle(
        id = "entity1",
        position = Vector2D(0.0, 0.0)
      ),
      "teamA"
    )
    
    val entity2 = addTeam(
      makeMovingEntityCircle(
        id = "entity2",
        position = Vector2D(3.0, 4.0)
      ),
      "teamB"
    )
    
    val teamA = makeTeam(entity1.teamId.value.value, Set(entity2.teamId.value.value))
    val teamB = makeTeam(entity2.teamId.value.value)
    
    val result = PhysicsUtil.nearestEnemy(entity1, List(entity1, entity2), List(teamA, teamB))

    result.value shouldBe entity2

  test("nearestEnemy should select the closest enemy when multiple enemies are present"):
    
    val entity = addTeam(
      makeMovingEntityCircle(
        id = "entity",
        position = Vector2D(0.0, 0.0)
      ),
      "teamA"
    )
    
    val farEnemy = addTeam(
      makeMovingEntityCircle(
        id = "farEnemy",
        position = Vector2D(10.0, 10.0)
      ),
      "teamB"
    )
    
    val closeEnemy = addTeam(
      makeMovingEntityCircle(
        id = "closeEnemy",
        position = Vector2D(3.0, 4.0)
      ),
      "teamB"
    )

    val teamA = makeTeam(entity.teamId.value.value, Set(closeEnemy.teamId.value.value))
    val teamB = makeTeam(farEnemy.teamId.value.value)
    
    val result = PhysicsUtil.nearestEnemy(entity, List(entity, farEnemy, closeEnemy), List(teamA, teamB))

    result.value shouldBe closeEnemy
