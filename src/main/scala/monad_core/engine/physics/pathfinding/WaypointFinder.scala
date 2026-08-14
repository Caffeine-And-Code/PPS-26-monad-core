package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.physics.pathfinding.PathRectangle.vertexes
import monad_core.engine.physics.utils.PhysicsUtil

private[pathfinding] object WaypointFinder:

  private val WaypointsNumber = 2

  def apply(start: Entity, target: Entity): List[Vector2D] =
    target.shape match
      case circle: Circle =>
        findWaypointsForCircle(start, target, circle)
      case rectangle: Rectangle =>
        findRectangleWaypoints(start, target, rectangle)

  private def findWaypointsForCircle(
      start: Entity,
      target: Entity,
      circle: Circle
  ): List[Vector2D] =

    val dx = start.position.x - target.position.x
    val dy = start.position.y - target.position.y

    val dist = PhysicsUtil.distance(start.position, target.position)

    val theta = math.atan2(dy, dx)

    val alpha = math.acos(circle.radius / dist)

    val angle1 = theta + alpha
    val angle2 = theta - alpha

    List(
      PointOnCircle(target.position, circle.radius, angle1),
      PointOnCircle(target.position, circle.radius, angle2)
    )

  private def angleScore(a: Vector2D, b: Vector2D): Double =
    (a dot b) / (a.magnitude * b.magnitude)

  private def findRectangleWaypoints(
      start: Entity,
      target: Entity,
      rectangle: Rectangle
  ): List[Vector2D] =

    val vertexes = rectangle.vertexes(target.position)

    val centerDirection = target.position - start.position

    vertexes
      .map { vertex =>
        vertex -> angleScore(centerDirection, vertex - start.position)
      }
      .sortBy(_._2)
      .take(WaypointsNumber)
      .map(_._1)
