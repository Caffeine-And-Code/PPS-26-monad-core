package monad_core.engine.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WeightTest extends AnyFunSuite with Matchers:

  test("zero is a valid weight"):
    val zeroWeight = 0

    val result = Weight(0)

    result.isRight shouldBe true
