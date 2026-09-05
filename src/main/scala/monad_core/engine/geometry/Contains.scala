package monad_core.engine.geometry

import monad_core.engine.model.Vector2D

/**
 * Computes whether a placed shape contains a point.
 *
 * @tparam A contained shape type
 */
trait Contains[A]:

  /**
   * Checks whether a point is inside or on the boundary of a placed shape.
   *
   * @param container a placed item representing the container
   * @param target point expressed in world coordinates
   * @return `true` when the point is inside the shape or on its boundary
   */
  def checkIfContains(container: Placed[A], target: Vector2D): Boolean

/** Provides containment syntax for placed shapes. */
object Contains:

  extension [A](container: Placed[A])

    /**
     * Checks whether this placed shape contains a point.
     *
     * @param target point coordinates
     * @param containsInstance containment implementation for the shape type
     * @return `true` when the point is inside the shape or on its boundary
     */
    infix def contains(target: Vector2D)(using containsInstance: Contains[A]): Boolean =
      containsInstance.checkIfContains(container, target)
