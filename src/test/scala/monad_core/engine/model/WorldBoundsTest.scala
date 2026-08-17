package monad_core.engine.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WorldBoundsTest extends AnyFunSuite with Matchers:

  test("WorldBounds should be created successfully with valid width and height"):
    val result = WorldBounds(100.0, 50.0).value
    
    result.upperLeft shouldBe Vector2D(0.0, 0.0)
    result.lowerRight shouldBe Vector2D(100.0, 50.0)

  test("WorldBounds should return an error when width is negative"):
    WorldBounds(-100.0, 50.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())

  test("WorldBounds should return an error when height is negative"):
    WorldBounds(100.0, -50.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())

  test("WorldBounds should return an error when both width and height are negative"):
    WorldBounds(-100.0, -50.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())

  test("WorldBounds should return an error when width is zero"):
    WorldBounds(0.0, 50.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())

  test("WorldBounds should return an error when height is zero"):
    WorldBounds(100.0, 0.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())

  test("WorldBounds should return an error when both width and height are zero"):
    WorldBounds(0.0, 0.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())

  test("WorldBounds should return an error when width is negative and height is zero"):
    WorldBounds(-100.0, 0.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())

  test("WorldBounds should return an error when width is zero and height is negative"):
    WorldBounds(0.0, -50.0) shouldBe Left(WorldBoundsCannotBeNegativeOrZero())
    
  test("WorldBounds should return a correct default instance"):
    val defaultBounds = WorldBounds.default
    
    defaultBounds.upperLeft shouldBe Vector2D(0.0, 0.0)
    defaultBounds.lowerRight shouldBe Vector2D(100.0, 100.0)