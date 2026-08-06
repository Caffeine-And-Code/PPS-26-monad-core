package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Circle
import monad_core.engine.model.Shape2D.Rectangle
import monad_core.engine.model.*
import monad_core.engine.physics.utils.PhysicsUtil

object WaypointFinder :
  def apply(start: Entity, target: Entity): List[Vector2D] =
    target.shape match
      case Circle(radius) =>
        findWaypointsForCircle(start, target)
      case Rectangle(height, length) =>
        findRectangleWaypoints(start, target)
  
  private def findWaypointsForCircle(start: Entity, target: Entity): List[Vector2D] = 
    
    val circle = target.shape.asInstanceOf[Circle]
    
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
    a.dot(b) / a.magnitude * b.magnitude
  
  private def findRectangleWaypoints(
                                      start: Entity,
                                      target: Entity
                                    ): List[Vector2D] = 

    val rectangle = target.shape.asInstanceOf[Rectangle]

    val vertices = rectangle.vertexes(target.position)

    val centerDirection = target.position - start.position

    vertices
      .map { vertex =>
        vertex -> angleScore(centerDirection, vertex - start.position)
      }
      .sortBy(_._2)
      .take(2)
      .map(_._1)