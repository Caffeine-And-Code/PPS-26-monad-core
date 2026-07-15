package engine.geometry

import engine.geometry.{Collision, Placed, ShapeCollision}
import engine.model.{Shape2D, Vector2D}
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class ShapeCollisionTest extends AnyFunSuite with Matchers:

  test("circle collides with another circle"):
    val cases = Table(
      (
        "firstCircle",
        "secondCircle",
        "collision"
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(3, 0), Shape2D.circle(2).value),
        Collision(Vector2D(1, 0), 1)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(0, 3), Shape2D.circle(2).value),
        Collision(Vector2D(0, 1), 1)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(-3, 0), Shape2D.circle(2).value),
        Collision(Vector2D(-1, 0), 1)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.circle(2).value),
        Placed(Vector2D(0, -3), Shape2D.circle(2).value),
        Collision(Vector2D(0, -1), 1)
      )
    )

    forAll(cases): (firstCircle, secondCircle, collision) =>

      val result = ShapeCollision.circleCollidesWithCircle.checkCollision(firstCircle, secondCircle)

      result shouldBe Some(collision)

  test("circle not collides with another circle"):
    val firstCircle = Placed(Vector2D(3, 3), Shape2D.circle(9).value)
    val secondCircle = Placed(Vector2D(15, 3), Shape2D.circle(2).value)

    val result = ShapeCollision.circleCollidesWithCircle.checkCollision(firstCircle, secondCircle)

    result shouldBe None

  test("rectangle collides with another rectangle"):
    val cases = Table(
      (
        "firstRectangle",
        "secondRectangle",
        "collision"
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(2.5, 0), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(1, 0), 2.5)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(0, 2.5), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(0, 1), 2.5)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(-2.5, 0), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(-1, 0), 2.5)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(5, 5).value),
        Placed(Vector2D(0, -2.5), Shape2D.rectangle(5, 5).value),
        Collision(Vector2D(0, -1), 2.5)
      )
    )

    forAll(cases): (firstRectangle, secondRectangle, collision) =>

      val result = ShapeCollision.rectangleCollidesWithRectangle.checkCollision(firstRectangle, secondRectangle)

      result shouldBe Some(collision)

  test("rectangle not collides with another rectangle"):
    val firstRectangle = Placed(Vector2D(0, 0), Shape2D.rectangle(5.9, 5.9).value)
    val secondRectangle = Placed(Vector2D(6, 0), Shape2D.rectangle(5.9, 5.9).value)

    val result = ShapeCollision.rectangleCollidesWithRectangle.checkCollision(firstRectangle, secondRectangle)

    result shouldBe None

  test("circle collides with a rectangle"):
    val cases = Table(
      (
        "circle",
        "rectangle",
        "collision"
      ),
      (
        Placed(Vector2D(6, 0), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(-1, 0), 1)
      ),
      (
        Placed(Vector2D(0, 6), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, -1), 1)
      ),
      (
        Placed(Vector2D(-6, 0), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(1, 0), 1)
      ),
      (
        Placed(Vector2D(0, -6), Shape2D.circle(4).value),
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, 1), 1)
      )
    )

    forAll(cases): (circle, rectangle, collision) =>

      val result = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle)

      result shouldBe Some(collision)

  test("circle not collides with another rectangle"):
    val circle = Placed(Vector2D(0, 0), Shape2D.circle(2.9).value)
    val rectangle = Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value)

    val result = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle)

    result shouldBe None

  test("circle collides with another rectangle too if it is fully inside it"):

    val cases = Table(
      (
        "circle",
        "rectangle",
        "collision"
      ),
      (
        Placed(Vector2D(7, 0), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(-1, 0), 2)
      ),
      (
        Placed(Vector2D(6, 1), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, -1), 2)
      ),
      (
        Placed(Vector2D(5, 0), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(1, 0), 2)
      ),
      (
        Placed(Vector2D(6, -1), Shape2D.circle(2).value),
        Placed(Vector2D(6, 0), Shape2D.rectangle(6, 6).value),
        Collision(Vector2D(0, 1), 2)
      )
    )

    forAll(cases): (circle, rectangle, collision) =>

      val result = ShapeCollision.circleCollidesWithRectangle.checkCollision(circle, rectangle)

      result shouldBe Some(collision)

  test("rectangle collides with a circle"):
    val cases = Table(
      (
        "rectangle",
        "circle",
        "collision"
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(6, 0), Shape2D.circle(4).value),
        Collision(Vector2D(1, 0), 1)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(0, 6), Shape2D.circle(4).value),
        Collision(Vector2D(0, 1), 1)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(-6, 0), Shape2D.circle(4).value),
        Collision(Vector2D(-1, 0), 1)
      ),
      (
        Placed(Vector2D(0, 0), Shape2D.rectangle(6, 6).value),
        Placed(Vector2D(0, -6), Shape2D.circle(4).value),
        Collision(Vector2D(0, -1), 1)
      )
    )

    forAll(cases): (rectangle, circle, collision) =>

      val result = ShapeCollision.rectangleCollidesWithCircle.checkCollision(rectangle, circle)

      result shouldBe Some(collision)