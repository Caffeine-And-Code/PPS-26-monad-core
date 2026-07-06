package units.engine.geometry

import engine.geometry.Collides.hasCollisionWithPlaced
import engine.geometry.{Collides, Collision, Placed}
import engine.model.Shape2D
import engine.model.Shape2D.Circle
import engine.model.Vector2D
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollidesTest extends AnyFunSuite with Matchers with MockFactory:

  test("the collisionWith method of a Placed entity uses a given Collides implementation"):
    val first = Placed(Vector2D(0, 0), Shape2D.circle(10).value)
    val second = Placed(Vector2D(5, 0), Shape2D.circle(10).value)
    val expectedCollision = Some(Collision(Vector2D(1, 0), 15))
    val collidesInstance = mock[Collides[Circle, Circle]]

    collidesInstance.collision
      .expects(first, second)
      .returning(expectedCollision)
      .once()

    val result = first.hasCollisionWithPlaced(second)(using collidesInstance)

    result shouldBe expectedCollision