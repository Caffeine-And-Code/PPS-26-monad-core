package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.*
import PathRectangle.vertexes
import monad_core.engine.physics.utils.PhysicsUtil
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WaypointFinderTest extends AnyFunSuite with Matchers:
  private val Start = Entity.circle("start", Vector2D(0, 0), 1).value

  private def signedAngle(from: Vector2D, to: Vector2D): Double =
    val cross = from.x * to.y - from.y * to.x
    math.atan2(cross, from dot to)

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

    val verticesByAngle = vertices.map { vertex =>
      vertex -> signedAngle(centerDirection, vertex - Start.position)
    }

    val expectedWaypoints = List(
      verticesByAngle.minBy(_._2)._1,
      verticesByAngle.maxBy(_._2)._1
    )

    val waypoints = WaypointFinder(Start, target)

    waypoints should contain theSameElementsAs expectedWaypoints

  test("rectangle waypoints should bracket a non-square obstacle"):
    val target = Entity.rectangle(
      "wide-target",
      Vector2D(12.5, 15.0),
      height = 2.0,
      length = 10.0
    ).value

    val centerDirection = target.position - Start.position
    val waypointAngles = WaypointFinder(Start, target).map { waypoint =>
      signedAngle(centerDirection, waypoint - Start.position)
    }

    waypointAngles.min should be < 0.0
    waypointAngles.max should be > 0.0
