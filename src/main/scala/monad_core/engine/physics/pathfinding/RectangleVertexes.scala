package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Rectangle
import monad_core.engine.model.*

object RectangleVertexes:

  extension (rectangle: Rectangle)

    def vertexes(position: Vector2D, rotation: Double = 0.0): List[Vector2D] =
      List(
        Vector2D(-rectangle.halfLength, -rectangle.halfHeight),
        Vector2D(rectangle.halfLength, -rectangle.halfHeight),
        Vector2D(rectangle.halfLength, rectangle.halfHeight),
        Vector2D(-rectangle.halfLength, rectangle.halfHeight)
      ).map(localVertex => position + localVertex.rotated(rotation))

    def upperVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).minBy(_.y)

    def lowerVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).maxBy(_.y)

    def leftVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).minBy(_.x)

    def rightVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).maxBy(_.x)

    def horizontalSize(position: Vector2D, rotation: Double = 0.0): Double =
      val leftVertex  = rectangle.leftVertex(position, rotation)
      val rightVertex = rectangle.rightVertex(position, rotation)
      rightVertex.x - leftVertex.x

    def verticalSize(position: Vector2D, rotation: Double = 0.0): Double =
      val upperVertex = rectangle.upperVertex(position, rotation)
      val lowerVertex = rectangle.lowerVertex(position, rotation)
      lowerVertex.y - upperVertex.y
