package integrations.engine.collision_detection

import engine.collision_detection.CollisionDetector
import engine.geometry.Collision
import engine.geometry.ShapeCollision.shapeCollidesWIthShape
import engine.geometry.ShapeContainment.shapeContainsPoint
import engine.model.*
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollisionDetectorTest extends AnyFunSuite with Matchers:

  test("detects a collision between locatables using the geometry implementation"):
    val first = Entity.circle("en1", Vector2D(0, 0), 5).value
    val second = Surface.circle("sur1", Vector2D(7, 0), 3).value
    val detector = CollisionDetector.fromGeometry

    val result = detector.collision(first, second)

    result shouldBe Some(Collision(Vector2D(1, 0), 1))

  test("detects the absence of collision between locatables using the geometry implementation"):
    val first = Entity.rectangle("en1", Vector2D(0, 0), 2, 2).value
    val second = Surface.rectangle("sur1", Vector2D(5, 0), 2, 2).value
    val detector = CollisionDetector.fromGeometry

    val result = detector.collision(first, second)

    result shouldBe None

  test("detects when a locatable is inside a container using the geometry implementation"):
    val target = Entity.circle("en1", Vector2D(1, 1), 1).value
    val container = Surface.rectangle("sur1", Vector2D(0, 0), 4, 4).value
    val detector = CollisionDetector.fromGeometry

    val result = detector.isInside(target, container)

    result shouldBe true

  test("detects when a locatable is outside a container using the geometry implementation"):
    val target = Entity.circle("en1", Vector2D(3, 0), 1).value
    val container = Surface.rectangle("sur1", Vector2D(0, 0), 4, 4).value
    val detector = CollisionDetector.fromGeometry

    val result = detector.isInside(target, container)

    result shouldBe false
