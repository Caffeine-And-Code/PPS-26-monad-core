package monad_core.performance.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class IterationCountTest extends AnyFunSuite with Matchers:

  test("an iteration count can be created from a positive value"):
    val value = 10

    val result = IterationCount.from(value)

    result.map(_.value) shouldBe Right(value)

  test("an iteration count cannot be created from zero"):
    val value = 0

    val result = IterationCount.from(value)

    result shouldBe Left(InvalidIterationCount(value))

  test("an iteration count cannot be created from a negative value"):
    val value = -1

    val result = IterationCount.from(value)

    result shouldBe Left(InvalidIterationCount(value))
