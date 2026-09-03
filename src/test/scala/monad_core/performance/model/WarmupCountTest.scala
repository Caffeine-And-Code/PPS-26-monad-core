package monad_core.performance.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WarmupCountTest extends AnyFunSuite with Matchers:

  test("WarmupCount accepts zero"):
    val result = WarmupCount.from(0)

    result.value.value shouldBe 0

  test("WarmupCount accepts a positive value"):
    val result = WarmupCount.from(2)

    result.value.value shouldBe 2

  test("WarmupCount rejects a negative value"):
    val result = WarmupCount.from(-1)

    result shouldBe Left(InvalidWarmupCount(-1))
