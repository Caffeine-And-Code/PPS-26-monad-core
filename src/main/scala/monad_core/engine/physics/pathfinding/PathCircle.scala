package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Circle
import monad_core.engine.model.Vector2D
import monad_core.engine.physics.pathfinding.PointOnCircle

private[physics] object PathCircle :
  extension (circle: Circle)
    def vertexes(position: Vector2D, n: Int): List[Vector2D] =
      
      (0 until n).map { i =>
        val angle = 2 * math.Pi * i / n
        PointOnCircle(position, circle.radius, angle)
      }.toList
    
    def verticalSize(): Double = circle.radius * 2
    
    def horizontalSize(): Double = circle.radius * 2