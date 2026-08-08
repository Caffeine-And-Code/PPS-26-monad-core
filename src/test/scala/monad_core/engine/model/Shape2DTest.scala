package monad_core.engine.model

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{HeightMustBeGreaterThanZero, LengthMustBeGreaterThanZero, RadiusMustBeGreaterThanZero, Shape2D}
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
    val invalidRadii = Seq(0, -1)

    invalidRadii.foreach: invalidRadius =>
      Shape2D.circle(invalidRadius) shouldBe Left(RadiusMustBeGreaterThanZero())

  test("cannot create a rectangle with invalid height and length"):
    val invalidLengths = Seq(0, -1)
    val validLength = 1
    val invalidHeights = Seq(0, -1)
    val validHeight = 1

    invalidHeights.foreach: invalidHeight =>
      Shape2D.rectangle(invalidHeight, validLength) shouldBe Left(HeightMustBeGreaterThanZero())
    invalidLengths.foreach: invalidLength =>
      Shape2D.rectangle(validHeight, invalidLength) shouldBe Left(LengthMustBeGreaterThanZero())
