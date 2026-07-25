package monad_core.engine.geometry

import monad_core.engine.geometry.{Placed, ShapeContainment}
import monad_core.engine.model.{Shape2D, Vector2D}
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ShapeContainmentTest extends AnyFunSuite with Matchers:

  test("circle contains a point"):
    val circle = Shape2D.circle(5).value
    val position = Vector2D(3, 4)
    val otherPosition = Vector2D(3, 4)

    val result = ShapeContainment.circleContainsPoint.checkIfContains(Placed(position, circle), otherPosition)

    result shouldBe true

  test("circle does not contain a point outside of it"):
    val circle = Shape2D.circle(5).value
    val position = Vector2D(0, 0)
    val otherPosition = Vector2D(6, 0)

    val result = ShapeContainment.circleContainsPoint.checkIfContains(Placed(position, circle), otherPosition)

    result shouldBe false

  test("rectangle contains a point"):
    val rectangle = Shape2D.rectangle(4, 6).value
    val position = Vector2D(3, 3)
    val otherPosition = Vector2D(3, 3)

    val result = ShapeContainment.rectangleContainsPoint.checkIfContains(Placed(position, rectangle), otherPosition)

    result shouldBe true

  test("rectangle does not contain a point outside of it"):
    val rectangle = Shape2D.rectangle(4, 6).value
    val position = Vector2D(3, 3)
    val otherPosition = Vector2D(7, 5)

    val result = ShapeContainment.rectangleContainsPoint.checkIfContains(Placed(position, rectangle), otherPosition)

    result shouldBe false
