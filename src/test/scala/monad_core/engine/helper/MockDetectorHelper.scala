package monad_core.engine.helper

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, Surface, Vector2D}
import org.scalamock.scalatest.MockFactory

private[engine] trait MockDetectorHelper:

  self: MockFactory =>

  def detectorWithoutCollision(): CollisionDetector =
    val detector = mock[CollisionDetector]

    detector.collision
      .expects(*, *)
      .returning(None)
      .anyNumberOfTimes()

    detector

  def detectorWithCollisions(
      collisions: Map[(String, String), (Vector2D, Double)]
  ): CollisionDetector =
    val detector = mock[CollisionDetector]

    detector.collision
      .expects(*, *)
      .onCall { (entity1, entity2) =>
        collisions.get((entity1.id.value, entity2.id.value)).map { case (normal, depth) =>
          Collision(normal, depth)
        }
      }
      .anyNumberOfTimes()

    detector

  def detectorWithContaining(
      contains: Map[(String, String), Boolean]
  ): CollisionDetector =
    val detector = mock[CollisionDetector]

    detector.isInside
      .expects(*, *)
      .onCall { (entity, surface) =>
        contains.getOrElse((entity.id.value, surface.id.value), false)
      }
      .anyNumberOfTimes()

    detector
