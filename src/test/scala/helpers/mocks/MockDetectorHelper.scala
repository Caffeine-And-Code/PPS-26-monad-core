package helpers.mocks

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.geometry.Collision
import monad_core.engine.model.Vector2D
import org.scalamock.scalatest.MockFactory

/** A trait for creating mock collision detectors for testing purposes. */
trait MockDetectorHelper:

  self: MockFactory =>

  /**
   * Creates a detector returning collisions from the supplied identifier pairs.
   *
   * @param collisions
   *   contact data indexed by ordered locatable identifiers
   * @return
   *   configured collision-detector mock
   */
  def detectorWithCollisions(
      collisions: Map[(String, String), (Vector2D, Double, Vector2D)]
  ): CollisionDetector =
    val detector = mock[CollisionDetector]

    detector.collision
      .expects(*, *)
      .onCall { (entity1, entity2) =>
        collisions.get((entity1.id.value, entity2.id.value)).map { case (normal, depth, point) =>
          Collision(normal, depth, point)
        }
      }
      .anyNumberOfTimes()
    
    detector.isInside
      .expects(*, *)
      .returning(false)
      .anyNumberOfTimes()

    detector

  /**
   * Creates a detector returning containment results from the supplied identifier pairs.
   *
   * @param contains
   *   containment results indexed by target and container identifiers
   * @return
   *   configured collision-detector mock
   */
  def detectorWithContaining(
      contains: Map[(String, String), Boolean]
  ): CollisionDetector =
    val detector = mock[CollisionDetector]

    detector.collision
      .expects(*, *)
      .returning(None)
      .anyNumberOfTimes()

    detector.isInside
      .expects(*, *)
      .onCall { (entity, surface) =>
        contains.getOrElse((entity.id.value, surface.id.value), false)
      }
      .anyNumberOfTimes()

    detector
