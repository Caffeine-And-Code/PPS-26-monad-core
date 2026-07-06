package units.engine.collision_detection

import engine.collision_detection.Colliding.hasCollisionWith
import engine.geometry.{Collides, Collision, Placed}
import engine.model.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollidingTest extends AnyFunSuite with Matchers with MockFactory:

  test("collisionWith returns the collision produced by Collides"):
    val entity = Entity.circle("en1", Vector2D(10, 20), 1).value
    val surface = Surface.rectangle("sur1", Vector2D(3, 4), 7, 9).value
    val expectedCollision = Some(Collision(Vector2D(-1, 0), 4.5))
    val collidesInstance = mock[Collides[Shape2D, Shape2D]]

    collidesInstance.collision
      .expects(Placed(entity.position, entity.shape), Placed(surface.position, surface.shape))
      .returning(expectedCollision)
      .once()

    val result = entity.hasCollisionWith(surface)(using collidesInstance)

    result shouldBe expectedCollision