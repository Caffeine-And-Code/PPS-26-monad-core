package engine.geometry

import engine.geometry.Contains.contains
import engine.model.*
import engine.model.Shape2D.{Circle, Rectangle}

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
