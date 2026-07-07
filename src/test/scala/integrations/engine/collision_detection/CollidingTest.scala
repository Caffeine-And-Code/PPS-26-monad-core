package integrations.engine.collision_detection

import engine.collision_detection.Colliding.hasCollisionWith
import engine.geometry.Collision
import engine.geometry.ShapeCollision.shapeCollidesWIthShape
import engine.model.{Entity, Vector2D}
import org.scalatest.EitherValues.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollidingTest extends AnyFunSuite with Inside with Matchers:

  test("can detect if a circle entity is colliding with another circle entity"):
    val firstCircle = Entity.circle("en1", Vector2D(0, 0), 5).value
    val secondCircle = Entity.circle("en2", Vector2D(5, 0), 5).value

    val result = firstCircle hasCollisionWith secondCircle

    result shouldBe Some(Collision(Vector2D(1, 0), 5))

  test("can detect if a rectangular entity is colliding with another rectangular entity"):
    val firstRectangle = Entity.rectangle("en1", Vector2D(0, 0), 5, 5).value
    val secondRectangle = Entity.rectangle("en2", Vector2D(5, 0), 5, 5).value

    val result = firstRectangle hasCollisionWith secondRectangle

    result shouldBe Some(Collision(Vector2D(1, 0), 0))