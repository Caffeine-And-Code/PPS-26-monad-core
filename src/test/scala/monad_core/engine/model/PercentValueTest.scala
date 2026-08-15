package monad_core.engine.model

import org.scalatest.Inside
import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Tables.Table

class PercentValueTest extends AnyFunSuite with Matchers with Inside:

  test("PercentValue creation with a value within [0, 100] succeeds"):
    val validValues = Table("value", 0, 1, 50, 99, 100)

    forAll(validValues): value =>
      val result = PercentValue(value)

      inside(result):
        case Right(percentValue) => percentValue.value should be(value)

  test("PercentValue creation with a value below 0 fails with PercentValueCannotExceedRange"):
    val result = PercentValue(-1)

    inside(result):
      case Left(error) => error shouldBe a[PercentValueCannotExceedRange]

  test("PercentValue creation with a value above 100 fails with PercentValueCannotExceedRange"):
    val result = PercentValue(101)

    inside(result):
      case Left(error) => error shouldBe a[PercentValueCannotExceedRange]
