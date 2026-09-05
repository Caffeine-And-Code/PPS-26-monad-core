package monad_core.engine.geometry

import monad_core.engine.model.{Locatable, Shape2D, Vector2D}

/**
 * Associates a shape with its position and orientation in world coordinates.
 *
 * @param center center of the shape
 * @param shape shape data
 * @param rotation rotation in degrees
 * @tparam A shape type
 */
final case class Placed[A](center: Vector2D, shape: A, rotation: Double = 0.0)

/** Converts engine model elements into geometry placements. */
object Placed:

  extension (locatable: Locatable)

    /**
     * Converts a locatable element into its geometric representation.
     *
     * @return placed shape using the locatable position, shape, and rotation
     */
    def placed: Placed[Shape2D] =
      Placed(locatable.position, locatable.shape, locatable.rotation)
