package monad_core.engine.model

import monad_core.engine.model.{HeightMustBeGreaterThanZero, LengthMustBeGreaterThanZero, RadiusMustBeGreaterThanZero, Shape2D}
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Shape2DTest extends AnyFunSuite with Matchers with Inside:

  test("can create a circle shape2D"):
    val validRadius = 2

    val circle = Shape2D.circle(validRadius)

    inside(circle):
      case Right(Circle(radius)) => radius shouldBe validRadius

  test("can create a rectangle shape2D"):
    val validHeight = 2
    val validLength = 2

    val rectangle = Shape2D.rectangle(validHeight, validLength)

    inside(rectangle):
      case Right(Rectangle(height, length)) =>
        height shouldBe validHeight
        length shouldBe validLength

  test("cannot create a circle with invalid radius"):
    val invalidRadius = 0

    val circle = Shape2D.circle(invalidRadius)

    circle shouldBe Left(RadiusMustBeGreaterThanZero())

  test("cannot create a rectangle with invalid height and length"):
    val invalidLength = 0
    val validLength = 1
    val invalidHeight = 0
    val validHeight = 1

    val rectangleWithInvalidHeight = Shape2D.rectangle(invalidHeight, validLength)
    val rectangleWithInvalidLength = Shape2D.rectangle(validHeight, invalidLength)

    rectangleWithInvalidHeight shouldBe Left(HeightMustBeGreaterThanZero())
    rectangleWithInvalidLength shouldBe Left(LengthMustBeGreaterThanZero())

