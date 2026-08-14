package monad_core.engine.model

import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class RGBValueTest extends AnyFunSuite with Matchers with Inside:

  test("RGBValue creation with a value within [0, 255] succeeds"):
    val validValues = Table("value", 0, 1, 127, 254, 255)

    forAll(validValues): value =>
      val result = RGBValue(value)

      inside(result):
        case Right(rgbValue) => rgbValue.value should be(value)

  test("RGBValue creation with a value below 0 fails with RGBValueCannotExceedRange"):
    val result = RGBValue(-1)

    inside(result):
      case Left(error) => error shouldBe a[RGBValueCannotExceedRange]

  test("RGBValue creation with a value above 255 fails with RGBValueCannotExceedRange"):
    val result = RGBValue(256)

    inside(result):
      case Left(error) => error shouldBe a[RGBValueCannotExceedRange]