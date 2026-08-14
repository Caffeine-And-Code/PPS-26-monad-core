package monad_core.engine.model

import org.scalatest.Inside
import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Tables.Table

class HueValueTest extends AnyFunSuite with Matchers with Inside:

  test("HueValue creation with a value within [0, 360] succeeds"):
    val validValues = Table("value", 0, 1, 180, 359, 360)

    forAll(validValues): value =>
      val result = HueValue(value)

      inside(result):
        case Right(hueValue) => hueValue.value should be(value)

  test("HueValue creation with a value below 0 fails with HueValueCannotExceedRange"):
    val result = HueValue(-1)

    inside(result):
      case Left(error) => error shouldBe a[HueValueCannotExceedRange]

  test("HueValue creation with a value above 360 fails with HueValueCannotExceedRange"):
    val result = HueValue(361)

    inside(result):
      case Left(error) => error shouldBe a[HueValueCannotExceedRange]