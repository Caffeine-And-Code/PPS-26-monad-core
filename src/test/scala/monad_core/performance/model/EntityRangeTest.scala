package monad_core.performance.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityRangeTest extends AnyFunSuite with Matchers:

  test("from stores the starting entity count"):
    val result = EntityRange.from(2, 8).value

    result.start.value shouldBe 2

  test("from stores the maximum entity count"):
    val result = EntityRange.from(2, 8).value

    result.maximum.value shouldBe 8

  test("from accepts a maximum equal to the starting count"):
    val result = EntityRange.from(2, 2)

    result shouldBe a[Right[?, ?]]

  test("from rejects an invalid starting entity count"):
    val result = EntityRange.from(0, 8)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("from rejects an invalid maximum entity count"):
    val result = EntityRange.from(2, 0)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("from rejects a maximum lower than the starting count"):
    val result = EntityRange.from(8, 2)

    result shouldBe Left(InvalidGrowthMaximum(8, 2))
