package monad_core.performance.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PositiveCountTest extends AnyFunSuite with Matchers:

  private val PositiveValue = 3

  test("EntityCount accepts a positive value"):
    val result = EntityCount.from(PositiveValue)

    result.value.value shouldBe PositiveValue

  test("EntityCount rejects zero"):
    val result = EntityCount.from(0)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("EntityCount rejects a negative value"):
    val result = EntityCount.from(-1)

    result shouldBe Left(InvalidPositiveCount("Entity count", -1))

  test("IterationCount accepts a positive value"):
    val result = IterationCount.from(PositiveValue)

    result.value.value shouldBe PositiveValue

  test("IterationCount rejects zero"):
    val result = IterationCount.from(0)

    result shouldBe Left(InvalidPositiveCount("Iteration count", 0))

  test("IterationCount rejects a negative value"):
    val result = IterationCount.from(-1)

    result shouldBe Left(InvalidPositiveCount("Iteration count", -1))
