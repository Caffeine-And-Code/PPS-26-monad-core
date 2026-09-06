package monad_core.engine.model

import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WeightTest extends AnyFunSuite with Matchers:

  test("can create an optional weight from a valid value"):
    val validWeight = Some(10)

    val weight = Weight.fromOption(validWeight)

    weight.value.map(_.value) shouldBe validWeight

  test("cannot create an optional weight from an invalid value"):
    val invalidWeight = Some(0)

    val weight = Weight.fromOption(invalidWeight)

    weight shouldBe Left(WeightCannotBeNegativeOrZero())

  test("zero is not a valid weight"):
    val zeroWeight = 0

    val result = Weight(0)

    result.isLeft shouldBe true
