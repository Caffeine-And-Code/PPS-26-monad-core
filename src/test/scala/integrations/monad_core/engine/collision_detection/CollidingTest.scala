package integrations.monad_core.engine.collision_detection

import monad_core.engine.collision_detection.Colliding.hasCollisionWith
import monad_core.engine.geometry.Collision
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.model.*
import org.scalatest.EitherValues.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollidingTest extends AnyFunSuite with Inside with Matchers:

  test("can detect if a circle entity is colliding with another circle entity"):
    val firstCircle  = Entity.circle("en1", Vector2D(0, 0), 5).value
    val secondCircle = Entity.circle("en2", Vector2D(5, 0), 5).value

    val result = firstCircle hasCollisionWith secondCircle

    result shouldBe Some(Collision(Vector2D(1, 0), 5, Vector2D(2.5, 0)))

  test("can detect if a circle entity is not colliding with another circle entity"):
    val firstCircle  = Entity.circle("en1", Vector2D(0, 0), 5).value
    val secondCircle = Entity.circle("en2", Vector2D(10.1, 0), 5).value

    val result = firstCircle hasCollisionWith secondCircle

    result shouldBe None

  test("can detect if a rectangular entity is colliding with another rectangular entity"):
    val firstRectangle  = Entity.rectangle("en1", Vector2D(0, 0), 5, 5).value
    val secondRectangle = Entity.rectangle("en2", Vector2D(5, 0), 5, 5).value

    val result = firstRectangle hasCollisionWith secondRectangle

    result shouldBe Some(Collision(Vector2D(1, 0), 0, Vector2D(2.5, 0)))

  test("can detect if a rectangular entity is not colliding with another rectangular entity"):
    val firstRectangle  = Entity.rectangle("en1", Vector2D(0, 0), 5, 5).value
    val secondRectangle = Entity.rectangle("en2", Vector2D(5, 0), 4.9, 4.9).value

    val result = firstRectangle hasCollisionWith secondRectangle

    result shouldBe None

  test("can detect if a circle entity is colliding with a rectangle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle    = Entity.circle("en2", Vector2D(6, 0), 6).value

    val result = circle hasCollisionWith rectangle

    result shouldBe Some(Collision(Vector2D(-1, 0), 3, Vector2D(3, 0)))

  test("can detect if a circle entity is not colliding with a rectangle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle    = Entity.circle("en2", Vector2D(9.1, 0), 6).value

    val result = circle hasCollisionWith rectangle

    result shouldBe None

  test("can detect if a circle entity inside a rectangle entity is colliding with it"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 6, 6).value
    val circle    = Entity.circle("en2", Vector2D(0, 1), 2).value

    val result = circle hasCollisionWith rectangle

    result shouldBe Some(Collision(Vector2D(0, -1), 4, Vector2D(0, 3)))

  test("can detect if a rectangle entity is colliding with a circle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle    = Entity.circle("en2", Vector2D(6, 0), 6).value

    val result = rectangle hasCollisionWith circle

    result shouldBe Some(Collision(Vector2D(1, 0), 3, Vector2D(3, 0)))

  test("can detect if a rectangle entity is not colliding with a circle entity"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 10, 6).value
    val circle    = Entity.circle("en2", Vector2D(9.1, 0), 6).value

    val result = rectangle hasCollisionWith circle

    result shouldBe None

  test("can detect collision between rotated locatables and return its collision point"):
    val rectangle = Entity.rectangle("en1", Vector2D(5, 5), 2, 6, 90).value
    val circle    = Entity.circle("en2", Vector2D(5, 9), 2).value

    val collision = circle.hasCollisionWith(rectangle).get

    collision.normalVector.x shouldBe 0.0 +- 0.1
    collision.normalVector.y shouldBe -1.0 +- 0.1
    collision.penetrationDepth shouldBe 1.0 +- 0.1
    collision.collisionPoint.x shouldBe 5.0 +- 0.1
    collision.collisionPoint.y shouldBe 8.0 +- 0.1

  test("can detect collision between two rotated rectangles"):
    val first  = Entity.rectangle("en1", Vector2D(0, 0), 4, 6, 25).value
    val second = Entity.rectangle("en2", Vector2D(2, 1), 3, 5, 345).value

    val collision = first.hasCollisionWith(second).get

    collision.normalVector.magnitude shouldBe 1.0 +- 1e-9
    collision.penetrationDepth should be >= 0.0
    collision.collisionPoint.x.isFinite shouldBe true
    collision.collisionPoint.y.isFinite shouldBe true

  test("can detect collision between concentric circles with a usable normal"):
    val first  = Entity.circle("en1", Vector2D(1, 1), 3).value
    val second = Entity.circle("en2", Vector2D(1, 1), 2).value

    val collision = first.hasCollisionWith(second).get

    collision.normalVector shouldBe Vector2D(1, 0)
    collision.penetrationDepth shouldBe 5.0

  test("rectangle-circle collision should reverse the circle-rectangle normal"):
    val rectangle = Entity.rectangle("en1", Vector2D(0, 0), 6, 6).value
    val circle    = Entity.circle("en2", Vector2D(0, 1), 2).value

    val circleCollision    = circle.hasCollisionWith(rectangle).get
    val rectangleCollision = rectangle.hasCollisionWith(circle).get

    rectangleCollision.normalVector shouldBe circleCollision.normalVector.flip
    rectangleCollision.penetrationDepth shouldBe circleCollision.penetrationDepth
    rectangleCollision.collisionPoint shouldBe circleCollision.collisionPoint
