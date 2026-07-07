package units.engine.geometry

import engine.geometry.{Collision, Placed, ShapeCollision}
import engine.model.{Shape2D, Vector2D}
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ShapeCollisionTest extends AnyFunSuite with Matchers {

  test("circle collides with another circle"):
    val firstCircle = Placed(Vector2D(3, 3), Shape2D.circle(10).value)
    val secondCircle = Placed(Vector2D(15, 3), Shape2D.circle(2).value)

    val result = ShapeCollision.circleCollidesWithCircle.collision(firstCircle, secondCircle)

    result shouldBe Some(Collision(Vector2D(1, 0), 0))
}
