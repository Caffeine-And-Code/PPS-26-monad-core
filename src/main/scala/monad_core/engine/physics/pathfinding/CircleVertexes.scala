package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Circle
import monad_core.engine.model.Vector2D
import monad_core.engine.physics.pathfinding.PointOnCircle

/** Geometric queries for a circle placed in world coordinates. */
private[pathfinding] object CircleVertexes:

  extension (circle: Circle)

    /**
     * Returns `vertexNumber` evenly spaced points along the circle circumference.
     *
     * @param position
     *  world coordinates of the circle's center
     * @param vertexNumber
     *  number of points to generate along the circumference
     * @return
     *  a list of `vertexNumber` points along the circle circumference
     */
    def vertexes(position: Vector2D, vertexNumber: Int): List[Vector2D] =
      (0 until vertexNumber).map { i =>
        val angle = 2 * math.Pi * i / vertexNumber
        PointOnCircle(position, circle.radius, angle)
      }.toList

    /**
     * Returns the circle diameter along the vertical axis.
     *
     * @return
     *   the circle diameter
     */
    def verticalSize(): Double = circle.radius * 2

    /**
     * Returns the circle diameter along the horizontal axis.
     *
     * @return
     *   the circle diameter
     */
    def horizontalSize(): Double = circle.radius * 2
