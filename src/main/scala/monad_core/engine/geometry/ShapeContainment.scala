package monad_core.engine.geometry

import monad_core.engine.geometry.Contains.contains
import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}

object ShapeContainment:

  given circleContainsPoint: Contains[Circle] with

    override def checkIfContains(container: Placed[Circle], point: Vector2D): Boolean =
      (point --> container.center) <= container.shape.radius

  given rectangleContainsPoint: Contains[Rectangle] with

    override def checkIfContains(container: Placed[Rectangle], point: Vector2D): Boolean =
      Interval(
        container.center.x - container.shape.halfLength,
        container.center.x + container.shape.halfLength
      ).contains(point.x)
        && Interval(
        container.center.y - container.shape.halfHeight,
        container.center.y + container.shape.halfHeight
      ).contains(point.y)

  given shapeContainsPoint: Contains[Shape2D] with

    override def checkIfContains(container: Placed[Shape2D], point: Vector2D): Boolean =
      container.shape match
        case circle: Circle =>
          Placed(container.center, circle) contains point
        case rectangle: Rectangle =>
          Placed(container.center, rectangle) contains point
