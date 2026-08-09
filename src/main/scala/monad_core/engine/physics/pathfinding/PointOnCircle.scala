package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Vector2D

private[pathfinding] object PointOnCircle:
  def apply(center: Vector2D, radius: Double, angle: Double): Vector2D =
    val x = center.x + radius * math.cos(angle)
    val y = center.y + radius * math.sin(angle)
    Vector2D(x, y)
