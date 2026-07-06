package engine.geometry

import engine.geometry.Contains.contains
import engine.model.Shape2D
import engine.model.Shape2D.{Circle, Rectangle}
import engine.model.*

object ShapeContainment:

  given circleContainsPoint: Contains[Circle] with

    override def contains(container: Placed[Circle], point: Vector2D): Boolean =
      (point --> container.center) <= container.value.radius

  given rectangleContainsPoint: Contains[Rectangle] with

    override def contains(container: Placed[Rectangle], point: Vector2D): Boolean =
      Interval(container.center.x - container.value.length / 2, container.center.x + container.value.length / 2).contains(point.x)
        && Interval(container.center.y - container.value.height / 2, container.center.y + container.value.height / 2).contains(point.y)
  
  given shapeContainsPoint: Contains[Shape2D] with

    override def contains(container: Placed[Shape2D], point: Vector2D): Boolean =
      container.value match
        case circle: Circle =>
          Placed(container.center, circle) contains point
        case rectangle: Rectangle =>
          Placed(container.center, rectangle) contains point
