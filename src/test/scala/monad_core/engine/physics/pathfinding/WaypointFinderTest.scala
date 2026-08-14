package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.physics.pathfinding.PathRectangle.vertexes
import monad_core.engine.physics.utils.PhysicsUtil
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WaypointFinderTest extends AnyFunSuite with Matchers:
  private val Start = Entity.circle("start", Vector2D(0, 0), 1).value

  test("WaypointFinder should return the correct waypoints for a circle target"):

    val target = Entity.circle("target", Vector2D(10, 10), 2).value

    val circleShape = target.shape match {
      case circle: Circle => circle
      case _              => fail("Target entity is not a circle")
    }

    val dx = Start.position.x - target.position.x
    val dy = Start.position.y - target.position.y

    val dist = PhysicsUtil.distance(Start.position, target.position)

    val theta = math.atan2(dy, dx)

    val alpha = math.acos(circleShape.radius / dist)

    val angle1 = theta + alpha
    val angle2 = theta - alpha

    val expectedWaypoints = List(
      PointOnCircle(target.position, circleShape.radius, angle1),
      PointOnCircle(target.position, circleShape.radius, angle2)
    )

    val waypoints = WaypointFinder(Start, target)

    waypoints should contain theSameElementsAs expectedWaypoints

  test("WaypointFinder should return the correct waypoints for a rectangle target"):
    val target = Entity.rectangle("target", Vector2D(10, 10), 4, 6).value

    val rectangleShape = target.shape match {
      case rectangle: Rectangle => rectangle
      case _                    => fail("Target entity is not a rectangle")
    }

    val vertices = rectangleShape.vertexes(target.position)

    val centerDirection = target.position - Start.position

    val expectedWaypoints = vertices
      .map { vertex =>
        vertex -> (centerDirection dot (vertex - Start.position))
          / (centerDirection.magnitude * (vertex - Start.position).magnitude)
      }
      .sortBy(_._2)
      .take(2)
      .map(_._1)

    val waypoints = WaypointFinder(Start, target)

    waypoints should contain theSameElementsAs expectedWaypoints
