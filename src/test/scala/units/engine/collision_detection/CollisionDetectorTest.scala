package units.engine.collision_detection

import engine.collision_detection.CollisionDetector
import engine.geometry.Placed.placed
import engine.geometry.{Collides, Collision, Contains}
import engine.model.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollisionDetectorTest extends AnyFunSuite with Matchers with MockFactory:

  test("collision delegates to the shape collision typeclass using placed locatables"):
    val first = Entity.circle("en1", Vector2D(0, 0), 5).value
    val second = Entity.rectangle("en2", Vector2D(6, 0), 4, 4).value
    val expectedCollision = Some(Collision(Vector2D(1, 0), 1))
    val collides = mock[Collides[Shape2D, Shape2D]]
    val contains = mock[Contains[Shape2D]]
    val detector = CollisionDetector.fromGeometry(using collides, contains)

    collides.collision
      .expects(first.placed, second.placed)
      .returning(expectedCollision)
      .once()

    val result = detector.collision(first, second)

    result shouldBe expectedCollision

  test("isInside delegates to the shape containment typeclass using the container shape and target position"):
    val target = Entity.circle("en1", Vector2D(1, 2), 1).value
    val container = Surface.rectangle("sur1", Vector2D(0, 0), 10, 10).value
    val collides = mock[Collides[Shape2D, Shape2D]]
    val contains = mock[Contains[Shape2D]]
    val detector = CollisionDetector.fromGeometry(using collides, contains)

    contains.contains
      .expects(container.placed, target.position)
      .returning(true)
      .once()

    val result = detector.isInside(target, container)

    result shouldBe true
