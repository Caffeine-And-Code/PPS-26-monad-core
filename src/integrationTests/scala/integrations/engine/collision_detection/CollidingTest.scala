package integrations.engine.collision_detection

import monad_core.engine.collision_detection.Colliding.hasCollisionWith
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, Vector2D}
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

  test("can detect if a circle entity is not colliding with another circle entity"):
    val firstCircle = Entity.circle("en1", Vector2D(0, 0), 5).value
    val secondCircle = Entity.circle("en2", Vector2D(10.1, 0), 5).value

    val result = firstCircle hasCollisionWith secondCircle

    result shouldBe None

  test("can detect if a rectangular entity is colliding with another rectangular entity"):
    val firstRectangle = Entity.rectangle("en1", Vector2D(0, 0), 5, 5).value
    val secondRectangle = Entity.rectangle("en2", Vector2D(5, 0), 5, 5).value

    val result = firstRectangle hasCollisionWith secondRectangle

    result shouldBe Some(Collision(Vector2D(1, 0), 0))

  test("can detect if a rectangular entity is not colliding with another rectangular entity"):
    val firstRectangle = Entity.rectangle("en1", Vector2D(0, 0), 5, 5).value
    val secondRectangle = Entity.rectangle("en2", Vector2D(5, 0), 4.9, 4.9).value

    val result = firstRectangle hasCollisionWith secondRectangle

    result shouldBe None

  test("can detect if a circle entity is colliding with a rectangle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle = Entity.circle("en2", Vector2D(6, 0), 6).value

    val result = circle hasCollisionWith rectangle

    result shouldBe Some(Collision(Vector2D(-1, 0), 3))

  test("can detect if a circle entity is not colliding with a rectangle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle = Entity.circle("en2", Vector2D(9.1, 0), 6).value

    val result = circle hasCollisionWith rectangle

    result shouldBe None

  test("can detect if a circle entity inside a rectangle entity is colliding with it"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 6, 6).value
    val circle = Entity.circle("en2", Vector2D(0, 1), 2).value

    val result = circle hasCollisionWith rectangle

    result shouldBe Some(Collision(Vector2D(0, -1), 2))


  test("can detect if a rectangle entity is colliding with a circle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle = Entity.circle("en2", Vector2D(6, 0), 6).value

    val result = rectangle hasCollisionWith circle

    result shouldBe Some(Collision(Vector2D(1, 0), 3))

  test("can detect if a rectangle entity is not colliding with a circle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle = Entity.circle("en2", Vector2D(9.1, 0), 6).value

    val result = rectangle hasCollisionWith circle

    result shouldBe None
