package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Rectangle
import monad_core.engine.model.Vector2D

private[physics] object PathRectangle:

  extension (rectangle: Rectangle)

    def vertexes(position: Vector2D): List[Vector2D] =
      List(
        Vector2D(
          position.x - rectangle.halfLength,
          position.y - rectangle.halfHeight
        ),
        Vector2D(
          position.x + rectangle.halfLength,
          position.y - rectangle.halfHeight
        ),
        Vector2D(
          position.x + rectangle.halfLength,
          position.y + rectangle.halfHeight
        ),
        Vector2D(
          position.x - rectangle.halfLength,
          position.y + rectangle.halfHeight
        )
      )

    def upperVertex(position: Vector2D): Vector2D =
      vertexes(position).minBy(_.y)

    def lowerVertex(position: Vector2D): Vector2D =
      vertexes(position).maxBy(_.y)

    def leftVertex(position: Vector2D): Vector2D =
      vertexes(position).minBy(_.x)

    def rightVertex(position: Vector2D): Vector2D =
      vertexes(position).maxBy(_.x)

    def horizontalSize(position: Vector2D): Double =
      val leftVertex  = rectangle.leftVertex(position)
      val rightVertex = rectangle.rightVertex(position)
      rightVertex.x - leftVertex.x

    def verticalSize(position: Vector2D): Double =
      val upperVertex = rectangle.upperVertex(position)
      val lowerVertex = rectangle.lowerVertex(position)
      lowerVertex.y - upperVertex.y
