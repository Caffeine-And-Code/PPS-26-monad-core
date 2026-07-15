package engine.collision_detection

import engine.collision_detection.Colliding.hasCollisionWith
import engine.collision_detection.CollisionDetector
import engine.geometry.Collision
import engine.model.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class CollidingTest extends AnyFunSuite with Matchers with MockFactory:

  test("hasCollisionWith returns the collision produced by CollisionDetector"):

    val cases = Table(
      ("firstEntity", "secondEntity", "expectedCollision"),
      (Entity.circle("en1", Vector2D(10, 20), 1).value, Entity.rectangle("en2", Vector2D(3, 4), 7, 9).value, Some(Collision(Vector2D(-1, 0), 4.5))),
      (Entity.rectangle("en1", Vector2D(10, 20), 1, 10).value, Entity.rectangle("en2", Vector2D(3, 4), 7, 9).value, Some(Collision(Vector2D(1, 0), 0))),
      (Entity.circle("en1", Vector2D(10, 20), 1).value, Entity.circle("en2", Vector2D(3, 4), 7).value, None)
    )

    forAll(cases): (firstEntity, secondEntity, expectedCollision) =>
      val detector = mock[CollisionDetector]

      detector.collision
        .expects(firstEntity, secondEntity)
        .returning(expectedCollision)
        .once()

      val result = firstEntity.hasCollisionWith(secondEntity)(using detector)

      result shouldBe expectedCollision
