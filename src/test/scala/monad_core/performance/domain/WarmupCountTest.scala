package monad_core.performance.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WarmupCountTest extends AnyFunSuite with Matchers:

  test("a warm-up count can be created from a positive value"):
    val value = 1

    val result = WarmupCount.from(value)

    result.map(_.value) shouldBe Right(value)

  test("a warm-up count can be created from zero"):
    val value = 0

    val result = WarmupCount.from(value)

    result.map(_.value) shouldBe Right(value)

  test("a warm-up count cannot be created from a negative value"):
    val value = -1

    val result = WarmupCount.from(value)

    result shouldBe Left(InvalidWarmupCount(value))
