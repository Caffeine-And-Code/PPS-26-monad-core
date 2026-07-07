package units.engine.geometry

import engine.geometry.{Collision, Placed, ShapeCollision}
import engine.model.{Shape2D, Vector2D}
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class ShapeCollisionTest extends AnyFunSuite with Matchers {

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

      val result = ShapeCollision.circleCollidesWithCircle.collision(firstCircle, secondCircle)

      result shouldBe Some(collision)

  test("circle not collides with another circle"):
    val firstCircle = Placed(Vector2D(3, 3), Shape2D.circle(9).value)
    val secondCircle = Placed(Vector2D(15, 3), Shape2D.circle(2).value)

    val result = ShapeCollision.circleCollidesWithCircle.collision(firstCircle, secondCircle)

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

      val result = ShapeCollision.rectangleCollidesWithRectangle.collision(firstRectangle, secondRectangle)

      result shouldBe Some(collision)
}
