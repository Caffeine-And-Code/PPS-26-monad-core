package engine.physics.core

import engine.model.{Vector2D, Weight, WeightCannotBeNegativeOrZero}
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsUtilTest extends AnyFunSuite with Matchers:

  val NegativeDt = -1L

  test("deltaSeconds should convert nanoseconds to seconds"):
    val nano = 1_500_000_000L
    val expectedSeconds = 1.5

    PhysicsUtil.deltaSeconds(nano) shouldBe Right(expectedSeconds)

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

    result shouldBe Right(expectedDisplacement)

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

    result shouldBe Right(expectedNextPosition)

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

    result shouldBe Right(expectedAcceleration)

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

    result shouldBe Right(expectedNextSpeed)

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

    result shouldBe Right(expectedFrictionSpeed)
    
  test("friction should not reverse speed"):
    val speedVectorX = 10.0
    val speedVectorY = 4.0
    val speedVector = Vector2D(speedVectorX, speedVectorY)
    val frictionIndex = 2.0
    val dt = 1_000_000_000L

    val result = PhysicsUtil.applyFriction(
      speed = speedVector,
      frictionIndex = frictionIndex,
      deltaTime = dt
    )

    result shouldBe Right(Vector2D(0.0, 0.0))

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
