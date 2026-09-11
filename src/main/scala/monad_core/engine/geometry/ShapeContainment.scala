package monad_core.engine.geometry

import monad_core.engine.geometry.Contains.contains
import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}

/**
 * Point-containment algorithms for the engine's circle and rectangle shapes.
 * Points on a shape boundary are considered contained.
 */
object ShapeContainment:

  /** Implementation of [[monad_core.engine.geometry.Contains]] for a circle. */
  given circleContainsPoint: Contains[Circle] with

    override def checkIfContains(container: Placed[Circle], point: Vector2D): Boolean =
      (point --> container.center) <= container.shape.radius

  /** Implementation of [[monad_core.engine.geometry.Contains]] for a rectangle. */
  given rectangleContainsPoint: Contains[Rectangle] with

    override def checkIfContains(container: Placed[Rectangle], point: Vector2D): Boolean =
      val localPoint = (point - container.center).rotated(-container.rotation)
      Interval(
        -container.shape.halfLength,
        container.shape.halfLength
      ).contains(localPoint.x)
      && Interval(
        -container.shape.halfHeight,
        container.shape.halfHeight
      ).contains(localPoint.y)

  /**
   * Dispatches containment checks according to the runtime type of [[monad_core.engine.model.Shape2D]].
   */
  given shapeContainsPoint: Contains[Shape2D] with

    override def checkIfContains(container: Placed[Shape2D], point: Vector2D): Boolean =
      container.shape match
        case circle: Circle =>
          Placed(container.center, circle, container.rotation) contains point
        case rectangle: Rectangle =>
          Placed(container.center, rectangle, container.rotation) contains point
