package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Vector2D

/** Calculates world points on a circle circumference. */
private[pathfinding] object PointOnCircle:

  /**
   * Calculates the point identified by one angle.
   *
   * @param center
   *   circle centre
   * @param radius
   *   circle radius
   * @param angle
   *   angle in radians
   * @return
   *   corresponding point in world coordinates
   */
  def apply(center: Vector2D, radius: Double, angle: Double): Vector2D =
    val x = center.x + radius * math.cos(angle)
    val y = center.y + radius * math.sin(angle)
    Vector2D(x, y)
