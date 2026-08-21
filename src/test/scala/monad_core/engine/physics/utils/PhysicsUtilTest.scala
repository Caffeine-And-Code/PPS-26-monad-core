package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityRectangle,
  makeMovingEntityCircle
}
import monad_core.engine.helper.DummyTeamHelper.{addTeam, makeTeam}
import monad_core.engine.helper.PhysicsConstantHelper.{DeltaTimeOneSecond, NegativeDt}
import monad_core.engine.model.*
import monad_core.engine.physics.core.{NegativeDeltaTime, ZeroMassError}
import monad_core.engine.physics.utils.PhysicsUtil
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsUtilTest extends AnyFunSuite with Matchers:

  private val UpperLeftCorner  = Vector2D(0.0, 0.0)
  private val LowerRightCorner = Vector2D(10.0, 10.0)

  test("deltaSeconds should convert nanoseconds to seconds"):
    val nano            = 1_500_000_000L
    val expectedSeconds = 1.5

    PhysicsUtil.timeLongToSeconds(nano).value shouldBe expectedSeconds

  test("deltaSeconds treats a negative delta time as zero"):

    PhysicsUtil.timeLongToSeconds(NegativeDt) shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("displacement should multiply speed by elapsed seconds"):
    val speed                = Vector2D(2.0, 4.0)
    val dt                   = 500_000_000L
    val dtS                  = PhysicsUtil.timeLongToSeconds(dt).value
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
    val position             = Vector2D(5.0, 2.0)
    val speed                = Vector2D(4.0, 6.0)
    val dt                   = 500_000_000L
    val dtS                  = PhysicsUtil.timeLongToSeconds(dt).value
    val expectedNextPosition = Vector2D(position.x + speed.x * dtS, position.y + speed.y * dtS)

    val result = PhysicsUtil.nextPosition(position = position, speed = speed, deltaTime = dt)

    result.value shouldBe expectedNextPosition

  test("nextPosition should return an error for negative delta time"):
    val position = Vector2D(5.0, 2.0)
    val speed    = Vector2D(4.0, 6.0)

    val result =
      PhysicsUtil.nextPosition(position = position, speed = speed, deltaTime = NegativeDt)

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("nextPosition should not return an error for out-of-bounds position"):
    val position = Vector2D(0.0, 0.0)
    val speed    = Vector2D(-1.0, -1.0)
    val dt       = 500_000_000L

    val result = PhysicsUtil.nextPosition(position = position, speed = speed, deltaTime = dt)

    result.isRight shouldBe true

  test("acceleration should be force divided by a (positive) mass"):
    val force                = Vector2D(10.0, 20.0)
    val massValue            = 2
    val expectedAcceleration = Vector2D(force.x / massValue.toDouble, force.y / massValue.toDouble)

    val result = PhysicsUtil.acceleration(
      force = force,
      mass = Some(Weight(massValue).value)
    )

    result.value shouldBe expectedAcceleration

  test("acceleration should return an error for missing mass"):
    val force = Vector2D(10.0, 20.0)

    val resultMissingMass = PhysicsUtil.acceleration(
      force = force,
      mass = None
    )

    resultMissingMass shouldBe Left(ZeroMassError())

  test("friction should reduce speed proportionally to elapsed time"):
    val speed                  = Vector2D(10.0, 4.0)
    val frictionIndex          = 2.0
    val dt                     = 500_000_000L
    val dtS                    = PhysicsUtil.timeLongToSeconds(dt).value
    val expectedFrictionFactor = 1.0 - frictionIndex * dtS
    val expectedFrictionSpeed =
      Vector2D(speed.x * expectedFrictionFactor, speed.y * expectedFrictionFactor)

    val result = PhysicsUtil.applyFriction(
      speed = speed,
      frictionIndex = frictionIndex,
      deltaTime = dt
    )

    result.value shouldBe expectedFrictionSpeed

  test("friction should not reverse speed"):
    val speed          = Vector2D(10.0, 4.0)
    val frictionIndex  = 2.0
    val dt             = 1_000_000_000L
    val expectedVector = Vector2D(0.0, 0.0)

    val result = PhysicsUtil.applyFriction(
      speed = speed,
      frictionIndex = frictionIndex,
      deltaTime = dt
    )

    result.value shouldBe expectedVector

  test("friction should return an error for negative delta time"):
    val speed         = Vector2D(10.0, 4.0)
    val frictionIndex = 2.0

    val result = PhysicsUtil.applyFriction(
      speed = speed,
      frictionIndex = frictionIndex,
      deltaTime = NegativeDt
    )

    result shouldBe Left(NegativeDeltaTime(NegativeDt))

  test("squaredDistance avoids an unnecessary square root"):
    val vector1       = Vector2D(1.0, 2.0)
    val vector2       = Vector2D(4.0, 6.0)
    val expectedValue = math.pow(vector2.x - vector1.x, 2) + math.pow(vector2.y - vector1.y, 2)

    val result = PhysicsUtil.squaredDistance(
      first = vector1,
      second = vector2
    )

    result shouldBe expectedValue

  test("distance should compute the Euclidean distance between two vectors"):
    val vector1       = Vector2D(1.0, 2.0)
    val vector2       = Vector2D(4.0, 6.0)
    val expectedValue = math.sqrt(PhysicsUtil.squaredDistance(vector1, vector2))

    val result = PhysicsUtil.distance(
      first = vector1,
      second = vector2
    )

    result shouldBe expectedValue

  test("reflectOnFixed should return the same speed if the velocity is along the normal"):
    val speed  = Vector2D(1.0, 0.0)
    val normal = Vector2D(1.0, 0.0)

    val result = PhysicsUtil.reflectOnFixed(
      speed = speed,
      normal = normal
    )

    result shouldBe speed

  test("reflectOnFixed should return an updated speed if the velocity is against the normal"):
    val speed         = Vector2D(-1.0, 0.0)
    val normal        = Vector2D(1.0, 0.0)
    val expectedSpeed = Vector2D(1.0, 0.0)

    val result = PhysicsUtil.reflectOnFixed(
      speed = speed,
      normal = normal
    )

    result shouldBe expectedSpeed

  test("reflectOnFixed should preserve a velocity tangent to the collision surface"):
    val speed  = Vector2D(0.0, 3.0)
    val normal = Vector2D(1.0, 0.0)

    PhysicsUtil.reflectOnFixed(speed, normal) shouldBe speed

  test("reflectOnFixed should return a new speed after collision with a fixed object"):
    val speed         = Vector2D(-3.0, 4.0)
    val normal        = Vector2D(1.0, 0.0)
    val expectedSpeed = Vector2D(3.0, 4.0)

    val result = PhysicsUtil.reflectOnFixed(
      speed = speed,
      normal = normal
    )

    result shouldBe expectedSpeed

  test(
    "pushMobileOverlappingFixed should return a new position after resolving overlap with a fixed object"
  ):
    val position         = Vector2D(5.0, 5.0)
    val normal           = Vector2D(1.0, 0.0)
    val penetrationDepth = 2.0
    val expectedPosition = Vector2D(7.0, 5.0)

    val result = PhysicsUtil.pushMobileOverlappingFixed(
      position = position,
      normal = normal,
      penetrationDepth = penetrationDepth
    )

    result shouldBe expectedPosition

  test("reflectOnMobile should return an accelerated speed if the velocity is along the normal"):
    val speed      = Vector2D(1.0, 0.0)
    val otherSpeed = Vector2D(2.0, 0.0)
    val normal     = Vector2D(1.0, 0.0)
    val mass       = Some(Weight(2).value)
    val massOther  = Some(Weight(3).value)

    val expectedSpeed = Vector2D(2.2, 0.0)

    val result = PhysicsUtil.reflectOnMobile(
      speed = speed,
      otherSpeed = otherSpeed,
      normal = normal,
      mass = mass,
      massOther = massOther
    )

    result.value shouldBe expectedSpeed

  test("reflectOnMobile should return an accelerated speed"):
    val speed      = Vector2D(1.0, 1.0)
    val otherSpeed = Vector2D(2.0, 0.0)
    val normal     = Vector2D(1.0, 0.0)
    val mass       = Some(Weight(2).value)
    val massOther  = Some(Weight(3).value)

    val expectedSpeed = Vector2D(2.2, 1.0)

    val result = PhysicsUtil.reflectOnMobile(
      speed = speed,
      otherSpeed = otherSpeed,
      normal = normal,
      mass = mass,
      massOther = massOther
    )

    result.value shouldBe expectedSpeed

  test("reflectOnMobile should preserve speed when the entities are separating"):
    val speed      = Vector2D(2.0, 0.0)
    val otherSpeed = Vector2D(1.0, 0.0)

    val result = PhysicsUtil.reflectOnMobile(
      speed = speed,
      otherSpeed = otherSpeed,
      normal = Vector2D(1.0, 0.0),
      mass = Some(Weight(1).value),
      massOther = Some(Weight(1).value)
    )

    result.value shouldBe speed

  test("reflectOnMobile should return a flipped speed for a force against the normal"):
    val speed      = Vector2D(-2.0, 0.0)
    val otherSpeed = Vector2D(2.0, 0.0)
    val normal     = Vector2D(1.0, 0.0)
    val mass       = Some(Weight(2).value)
    val massOther  = Some(Weight(3).value)

    val result = PhysicsUtil.reflectOnMobile(
      speed = speed,
      otherSpeed = otherSpeed,
      normal = normal,
      mass = mass,
      massOther = massOther
    )

    result.value.x >= 0.0 shouldBe true
    result.value.y shouldBe 0.0

  test("reflectOnMobile should return an error for missing mass"):
    val speed      = Vector2D(1.0, 0.0)
    val otherSpeed = Vector2D(2.0, 0.0)
    val normal     = Vector2D(1.0, 0.0)
    val mass       = None
    val massOther  = Some(Weight(3).value)

    val result = PhysicsUtil.reflectOnMobile(
      speed = speed,
      otherSpeed = otherSpeed,
      normal = normal,
      mass = mass,
      massOther = massOther
    )

    result shouldBe Left(ZeroMassError())

  test("reflectOnMobile should return an error for missing other mass"):
    val speed      = Vector2D(1.0, 0.0)
    val otherSpeed = Vector2D(2.0, 0.0)
    val normal     = Vector2D(1.0, 0.0)
    val mass       = Some(Weight(2).value)
    val massOther  = None

    val result = PhysicsUtil.reflectOnMobile(
      speed = speed,
      otherSpeed = otherSpeed,
      normal = normal,
      mass = mass,
      massOther = massOther
    )

    result shouldBe Left(ZeroMassError())

  test(
    "pushMobileOverlappingMobile should return a new position after resolving overlap with another mobile object"
  ):
    val position         = Vector2D(5.0, 5.0)
    val normal           = Vector2D(1.0, 0.0)
    val penetrationDepth = 2.0
    val massValue        = 2.0
    val massOtherValue   = 3.0
    val mass             = Some(Weight(massValue.toInt).value)
    val massOther        = Some(Weight(massOtherValue.toInt).value)
    val expectedPosition =
      position + (normal * penetrationDepth * (massValue / (massValue + massOtherValue)))

    val result = PhysicsUtil.pushMobileOverlappingMobile(
      position = position,
      normal = normal,
      penetrationDepth = penetrationDepth,
      mass = mass,
      massOther = massOther
    )

    result.value shouldBe expectedPosition

  test("pushMobileOverlappingMobile should return an error for missing mass"):
    val position         = Vector2D(5.0, 5.0)
    val normal           = Vector2D(1.0, 0.0)
    val penetrationDepth = 2.0
    val mass             = None
    val massOther        = Some(Weight(3).value)

    val result = PhysicsUtil.pushMobileOverlappingMobile(
      position = position,
      normal = normal,
      penetrationDepth = penetrationDepth,
      mass = mass,
      massOther = massOther
    )

    result shouldBe Left(ZeroMassError())

  test("pushMobileOverlappingMobile should return an error for missing other mass"):
    val position         = Vector2D(5.0, 5.0)
    val normal           = Vector2D(1.0, 0.0)
    val penetrationDepth = 2.0
    val mass             = Some(Weight(2).value)
    val massOther        = None

    val result = PhysicsUtil.pushMobileOverlappingMobile(
      position = position,
      normal = normal,
      penetrationDepth = penetrationDepth,
      mass = mass,
      massOther = massOther
    )

    result shouldBe Left(ZeroMassError())

  test("nearestEnemy should return None when the entity has no team"):
    val entity = makeMovingEntityCircle(
      id = "entity1"
    )

    val result = PhysicsUtil.nearestEnemy(entity, List(entity), List())

    result shouldBe None

  test("nearestEnemy should return None when the entity's team is not found"):
    val entity = addTeam(
      makeMovingEntityCircle(
        id = "entity1"
      ),
      "teamA"
    )

    val result = PhysicsUtil.nearestEnemy(entity, List(entity), List())

    result shouldBe None

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

    val result =
      PhysicsUtil.nearestEnemy(entity, List(entity, farEnemy, closeEnemy), List(teamA, teamB))

    result.value shouldBe closeEnemy

  test("timeLongToSeconds should accept zero delta time"):
    PhysicsUtil.timeLongToSeconds(0L) shouldBe Right(0.0)

  test("nextPosition should allow an entity to reach the lower-right boundary"):
    val position = Vector2D(9.0, 9.0)
    val speed    = Vector2D(1.0, 1.0)

    PhysicsUtil.nextPosition(position, speed, 1_000_000_000L) shouldBe Right(LowerRightCorner)

  test("nearestEnemy should ignore candidates without a team"):
    val entity = addTeam(
      makeMovingEntityCircle(id = "entity1", position = Vector2D(0.0, 0.0)),
      "teamA"
    )
    val unassignedCandidate = makeMovingEntityCircle(
      id = "unassigned",
      position = Vector2D(1.0, 0.0)
    )
    val team = makeTeam(entity.teamId.value.value, Set("teamB"))

    PhysicsUtil.nearestEnemy(
      entity,
      List(entity, unassignedCandidate),
      List(team)
    ) shouldBe None

  test("applyAngularFriction should reduce angular speed with the same friction factor"):
    val result = PhysicsUtil.applyAngularFriction(
      angularSpeed = 120.0,
      frictionIndex = 0.25,
      deltaTime = DeltaTimeOneSecond
    )

    result.value shouldBe 90.0