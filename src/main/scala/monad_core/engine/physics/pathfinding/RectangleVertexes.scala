package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Rectangle
import monad_core.engine.model.*

/** Geometric queries for a rectangle placed and rotated in world coordinates. */
private[engine] object RectangleVertexes:

  extension (rectangle: Rectangle)

    /**
     * Returns the four rectangle vertices in world coordinates.
     *
     * @param position
     *  world coordinates of the rectangle's center
     * @param rotation
     *  rotation of the rectangle in degrees
     * @return
     *  the four rectangle vertices in world coordinates
     */
    def vertexes(position: Vector2D, rotation: Double = 0.0): List[Vector2D] = {
      val upperRight = Vector2D(-rectangle.halfLength, -rectangle.halfHeight)
      val upperLeft  = Vector2D(rectangle.halfLength, -rectangle.halfHeight)
      List(
        upperRight,
        upperLeft,
        upperRight.flip,
        upperLeft.flip
      ).map(localVertex => position + localVertex.rotated(rotation))
    }

    /**
     * Returns the higher vertex.
     *
     * @param position
     *  world coordinates of the rectangle's center
     * @param rotation
     *  rotation of the rectangle in degrees
     * @return
     *  the higher vertex
     */
    def upperVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).minBy(_.y)

    /**
     * Returns the lower vertex.
     *
     * @param position
     *  world coordinates of the rectangle's center
     * @param rotation
     *  rotation of the rectangle in degrees
     * @return
     *  the lower vertex
     */
    def lowerVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).maxBy(_.y)

    /**
     * Returns the most left vertex.
     *
     * @param position
     *  world coordinates of the rectangle's center
     * @param rotation
     *  rotation of the rectangle in degrees
     * @return
     *  the most left vertex
     */
    def leftVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).minBy(_.x)

    /**
     * Returns the most right vertex.
     *
     * @param position
     *  world coordinates of the rectangle's center
     * @param rotation
     *  rotation of the rectangle in degrees
     * @return
     *  the most right vertex
     */
    def rightVertex(position: Vector2D, rotation: Double = 0.0): Vector2D =
      vertexes(position, rotation).maxBy(_.x)

    /**
     * Returns the width of the rotated rectangle's axis-aligned bounds.
     *
     * @param position
     *  world coordinates of the rectangle's center
     * @param rotation
     *  rotation of the rectangle in degrees
     * @return
     *  the width of the rotated rectangle's axis-aligned bounds
     */
    def horizontalSize(position: Vector2D, rotation: Double = 0.0): Double =
      val leftVertex  = rectangle.leftVertex(position, rotation)
      val rightVertex = rectangle.rightVertex(position, rotation)
      rightVertex.x - leftVertex.x

    /**
     * Returns the height of the rotated rectangle's axis-aligned bounds.
     *
     * @param position
     *  world coordinates of the rectangle's center
     * @param rotation
     *  rotation of the rectangle in degrees
     * @return
     *  the height of the rotated rectangle's axis-aligned bounds
     */
    def verticalSize(position: Vector2D, rotation: Double = 0.0): Double =
      val upperVertex = rectangle.upperVertex(position, rotation)
      val lowerVertex = rectangle.lowerVertex(position, rotation)
      lowerVertex.y - upperVertex.y
