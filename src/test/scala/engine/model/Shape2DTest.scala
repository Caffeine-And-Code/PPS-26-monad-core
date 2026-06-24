package engine.model

import engine.model.Shape2D.{Circle, Rectangle}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Shape2DTest extends AnyFunSuite with Matchers:

  test("can create a circle shape2D"):
    val validRadius = 2

    val circle = Shape2D.circle(validRadius)

    circle shouldBe Circle(validRadius)

  test("can create a rectangle shape2D"):
    val validHeight = 2;
    val validLength = 2;

    val rectangle = Shape2D.rectangle(validHeight, validLength)

    rectangle shouldBe Rectangle(validHeight, validLength)

  test("cannot create a circle with invalid radius"):
    val invalidRadius = 0

    an [IllegalArgumentException] shouldBe thrownBy:
      Shape2D.circle(invalidRadius)

