package units.engine.geometry

import engine.geometry.Contains.contains
import engine.geometry.Placed
import engine.geometry.ShapeContainment.given
import engine.model.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ShapeContainmentTest extends AnyFunSuite with Matchers:

  test("circle contains a point"):
    val circle = Shape2D
      .circle(5)
    val position = Vector2D(3, 4)
    val otherPosition = Vector2D(3, 4)

    val result = for {
      circle <- circle
    } yield Placed(position, circle) contains otherPosition

    result shouldBe Right(true)

  test("circle does not contain a point outside of it"):
    val circle = Shape2D
      .circle(5)
    val position = Vector2D(0, 0)
    val otherPosition = Vector2D(6, 0)

    val result = for {
      circle <- circle
    } yield Placed(position, circle) contains otherPosition

    result shouldBe Right(false)

  test("rectangle contains a point"):
    val rectangle = Shape2D
      .rectangle(4, 6)
    val position = Vector2D(3, 3)
    val otherPosition = Vector2D(3, 3)

    val result = for {
      rectangle <- rectangle
    } yield Placed(position, rectangle) contains otherPosition

    result shouldBe Right(true)

  test("rectangle does not contain a point outside of it"):
    val rectangle = Shape2D
      .rectangle(4, 6)
    val position = Vector2D(3, 3)
    val otherPosition = Vector2D(7, 5)

    val result = for {
      rectangle <- rectangle
    } yield Placed(position, rectangle) contains otherPosition

    result shouldBe Right(false)
