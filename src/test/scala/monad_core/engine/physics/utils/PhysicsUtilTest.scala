package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.helper.DummyEntityHelper.{
  makeFixedEntityCircle,
  makeFixedEntityRectangle,
  makeMovingEntityCircle,
  makeMovingEntityRectangle
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

  private val HorizontalCollision = Collision(
    normalVector = Vector2D(1.0, 0.0),
    penetrationDepth = 0.0,
    collisionPoint = Vector2D(0.0, 0.0)
  )

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
      position + (normal * penetrationDepth * (massOtherValue / (massValue + massOtherValue)))

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

  test("speedAtPoint should combine linear and angular speed"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(1.0, 1.0),
      speed = Vector2D(2.0, 3.0)
    ).withAngularSpeed(90.0)

    val result = PhysicsUtil.speedAtPoint(entity, Vector2D(2.0, 1.0))

    result.x shouldBe 2.0 +- 1e-9
    result.y shouldBe (3.0 + math.Pi / 2.0) +- 1e-9

  test("collisionResponse should split an off-center impulse between translation and rotation"):
    val entity = makeFixedEntityRectangle(
      position = Vector2D(5.0, 5.0),
      width = 4.0,
      height = 2.0
    ).withSpeed(Vector2D(-1.0, 0.0)).withAngularSpeed(0.0).withWeight(1).value
    val wall = makeFixedEntityRectangle(id = "wall")
    val collision = Collision(
      normalVector = Vector2D(1.0, 0.0),
      penetrationDepth = 0.0,
      collisionPoint = Vector2D(4.0, 7.0)
    )

    val (speedChange, angularSpeedChange) =
      PhysicsUtil.collisionResponse(entity, wall, collision).value
    val inertia = (4.0 * 4.0 + 2.0 * 2.0) / 12.0
    val impulse = 2.0 / 3.4

    speedChange.x shouldBe impulse +- 1e-9
    speedChange.y shouldBe 0.0 +- 1e-9
    angularSpeedChange shouldBe math.toDegrees(-2.0 * impulse / inertia) +- 1e-9

  test("applyAngularFriction should preserve angular speed with zero delta time"):
    PhysicsUtil.applyAngularFriction(45.0, 0.5, 0L).value shouldBe 45.0

  test("applyAngularFriction should not reverse angular speed"):
    PhysicsUtil
      .applyAngularFriction(45.0, 2.0, DeltaTimeOneSecond)
      .value shouldBe 0.0

  test("applyAngularFriction should preserve the sign of a negative angular speed"):
    PhysicsUtil
      .applyAngularFriction(-120.0, 0.25, DeltaTimeOneSecond)
      .value shouldBe -90.0

  test("applyAngularFriction should reject a negative delta time"):
    PhysicsUtil.applyAngularFriction(45.0, 0.5, NegativeDt) shouldBe Left(
      NegativeDeltaTime(NegativeDt)
    )

  test("speedAtPoint should return linear speed at the center of mass"):
    val entity = makeMovingEntityCircle(
      position = Vector2D(2.0, 3.0),
      speed = Vector2D(4.0, 5.0)
    ).withAngularSpeed(180.0)

    PhysicsUtil.speedAtPoint(entity, entity.position) shouldBe Vector2D(4.0, 5.0)

  test("speedAtPoint should return zero for a fixed entity"):
    val entity = makeFixedEntityCircle(position = Vector2D(2.0, 3.0))

    PhysicsUtil.speedAtPoint(entity, Vector2D(10.0, 10.0)) shouldBe Vector2D(0.0, 0.0)

  test("speedAtPoint should compute speed for a rotation-only entity"):
    val entity = makeFixedEntityCircle().withAngularSpeed(90.0)

    val result = PhysicsUtil.speedAtPoint(entity, Vector2D(0.0, 2.0))

    result.x shouldBe -math.Pi +- 1e-9
    result.y shouldBe 0.0 +- 1e-9

  test("speedAtPoint should respect clockwise rotation"):
    val entity = makeFixedEntityCircle().withAngularSpeed(-90.0)

    val result = PhysicsUtil.speedAtPoint(entity, Vector2D(2.0, 0.0))

    result.x shouldBe 0.0 +- 1e-9
    result.y shouldBe -math.Pi +- 1e-9

  test("collisionResponse should return an error when entity mass is missing"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0)).withAngularSpeed(0.0)
    val wall   = makeFixedEntityRectangle(id = "wall")

    PhysicsUtil.collisionResponse(entity, wall, HorizontalCollision) shouldBe Left(ZeroMassError())

  test("collisionResponse should return no impulse for separating contact points"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(1.0, 0.0))
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)
    val wall = makeFixedEntityRectangle(id = "wall")

    PhysicsUtil.collisionResponse(entity, wall, HorizontalCollision).value shouldBe
      (Vector2D(0.0, 0.0) -> 0.0)

  test("collisionResponse should not create torque for a centered impulse"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(0.0, 0.0),
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(1).value.withAngularSpeed(0.0)
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(-1.0, 0.0))

    val (speedChange, angularSpeedChange) =
      PhysicsUtil.collisionResponse(entity, wall, collision).value

    speedChange shouldBe Vector2D(2.0, 0.0)
    angularSpeedChange shouldBe 0.0

  test("collisionResponse should respect a locked translation degree of freedom"):
    val entity = makeFixedEntityRectangle()
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)
    val other = makeMovingEntityCircle(
      id = "other",
      speed = Vector2D(1.0, 0.0)
    ).withWeight(1).value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 1.0))

    val (speedChange, angularSpeedChange) =
      PhysicsUtil.collisionResponse(entity, other, collision).value

    speedChange shouldBe Vector2D(0.0, 0.0)
    angularSpeedChange should not be 0.0

  test("collisionResponse should respect a locked rotation degree of freedom"):
    val entity    = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0)).withWeight(1).value
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, 2.0))

    val (_, angularSpeedChange) = PhysicsUtil.collisionResponse(entity, wall, collision).value

    angularSpeedChange shouldBe 0.0

  test("collisionResponse should support a fixed other entity without mass"):
    val entity = makeMovingEntityRectangle(speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
      .withAngularSpeed(0.0)
    val wall = makeFixedEntityRectangle(id = "wall")

    PhysicsUtil.collisionResponse(entity, wall, HorizontalCollision).isRight shouldBe true

  test("collisionResponse should conserve kinetic energy against a fixed body"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(5.0, 5.0),
      width = 4.0,
      height = 2.0,
      speed = Vector2D(-1.0, 0.0)
    ).withWeight(1).value.withAngularSpeed(0.0)
    val wall      = makeFixedEntityRectangle(id = "wall")
    val collision = Collision(Vector2D(1.0, 0.0), 0.0, Vector2D(4.0, 7.0))
    val (speedChange, angularChange) =
      PhysicsUtil.collisionResponse(entity, wall, collision).value
    val finalSpeed        = entity.speed.value + speedChange
    val finalAngularSpeed = math.toRadians(angularChange)
    val inertia           = (4.0 * 4.0 + 2.0 * 2.0) / 12.0
    val finalEnergy =
      finalSpeed.magnitude * finalSpeed.magnitude / 2.0 +
        inertia * finalAngularSpeed * finalAngularSpeed / 2.0

    finalEnergy shouldBe 0.5 +- 1e-9

  test("collisionResponse should include both entities' translational inverse masses"):
    val entity = makeMovingEntityCircle(speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
    val other = makeMovingEntityCircle(id = "other", speed = Vector2D(1.0, 0.0))
      .withWeight(1)
      .value

    val (speedChange, _) =
      PhysicsUtil.collisionResponse(entity, other, HorizontalCollision).value

    speedChange shouldBe Vector2D(2.0, 0.0)

  test("collisionResponse should not add inverse mass for locked entity translation"):
    val radius = 1.0
    val entity = makeFixedEntityCircle(radius = radius)
      .withAngularSpeed(0.0)
      .withWeight(1)
      .value
    val other = makeMovingEntityCircle(
      id = "other",
      radius = radius,
      speed = Vector2D(1.0, 0.0)
    )
      .withWeight(1)
      .value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val (_, angularSpeedChange) =
      PhysicsUtil.collisionResponse(entity, other, collision).value
    val inertia         = radius * radius / 2.0
    val expectedImpulse = 2.0 / (1.0 + radius * radius / inertia)

    angularSpeedChange shouldBe math.toDegrees(-radius * expectedImpulse / inertia) +- 1e-9

  test("collisionResponse should not add rotational inverse mass for locked entity rotation"):
    val radius = 1.0
    val entity = makeMovingEntityCircle(radius = radius, speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
    val other     = makeFixedEntityCircle(id = "other", radius = radius)
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val (speedChange, angularSpeedChange) =
      PhysicsUtil.collisionResponse(entity, other, collision).value

    speedChange shouldBe Vector2D(2.0, 0.0)
    angularSpeedChange shouldBe 0.0

  test("collisionResponse should include the other entity's rotational inverse mass"):
    val radius = 1.0
    val entity = makeMovingEntityCircle(radius = radius, speed = Vector2D(-1.0, 0.0))
      .withWeight(1)
      .value
    val other = makeFixedEntityCircle(id = "other", radius = radius)
      .withAngularSpeed(0.0)
      .withWeight(1)
      .value
    val collision = HorizontalCollision.copy(collisionPoint = Vector2D(0.0, radius))

    val (speedChange, _) = PhysicsUtil.collisionResponse(entity, other, collision).value
    val inertia          = radius * radius / 2.0
    val expectedImpulse  = 2.0 / (1.0 + radius * radius / inertia)

    speedChange.x shouldBe expectedImpulse +- 1e-9
    speedChange.y shouldBe 0.0

  test("collisionResponse should return zero when all effective inverse masses are zero"):
    val entity = makeFixedEntityCircle()
      .withAngularSpeed(0.0)
      .withWeight(1)
      .value
    val other = makeFixedEntityCircle(id = "other")

    PhysicsUtil.collisionResponse(entity, other, HorizontalCollision).value shouldBe
      (Vector2D(0.0, 0.0) -> 0.0)

  test("pushMobileOverlappingMobile should split overlap equally for equal masses"):
    val result = PhysicsUtil.pushMobileOverlappingMobile(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      4.0,
      Some(Weight(2).value),
      Some(Weight(2).value)
    )

    result.value shouldBe Vector2D(2.0, 0.0)

  test("pushMobileOverlappingMobile should move a heavier entity less"):
    val heavy = PhysicsUtil.pushMobileOverlappingMobile(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      4.0,
      Some(Weight(3).value),
      Some(Weight(1).value)
    )
    val light = PhysicsUtil.pushMobileOverlappingMobile(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      4.0,
      Some(Weight(1).value),
      Some(Weight(3).value)
    )

    heavy.value.x should be < light.value.x

  test("pushMobileOverlappingMobile should preserve position for zero penetration"):
    val position = Vector2D(3.0, 4.0)

    PhysicsUtil
      .pushMobileOverlappingMobile(
        position,
        Vector2D(1.0, 0.0),
        0.0,
        Some(Weight(1).value),
        Some(Weight(1).value)
      )
      .value shouldBe position
