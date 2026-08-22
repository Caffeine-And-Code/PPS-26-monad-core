package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.physics.core.{RayIntersectedAMissingEntity, RayIntersectedNothing}
import monad_core.engine.helper.DummyEntityHelper.{
  makeMovingEntityCircle,
  makeMovingEntityRectangle
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RayCastTest extends AnyFunSuite with Matchers:

  private val From: Entity = makeMovingEntityCircle(
    id = "from",
    position = Vector2D(1.0, 1.0),
    radius = 1.0
  )

  private val To: Entity = makeMovingEntityCircle(
    id = "to",
    position = Vector2D(10.0, 10.0),
    radius = 0.5
  )

  private def rawToActualWaypoint(raw: Vector2D, obstacle: Entity): Vector2D =
    val clearance = RayCast.hunterRadius(From) + WayPointDisplacement
    val direction = raw - obstacle.position

    obstacle.shape match
      case _: Rectangle =>
        val localDirection = direction.rotated(-obstacle.rotation)
        val localInflation = Vector2D(
          math.signum(localDirection.x),
          math.signum(localDirection.y)
        ) * clearance
        raw + localInflation.rotated(obstacle.rotation)
      case _: Circle =>
        raw + direction.normalized * clearance

  private val UpperLeftCorner: Vector2D  = Vector2D(-20.0, -20.0)
  private val LowerRightCorner: Vector2D = Vector2D(40.0, 40.0)

  private val TightUpperLeftCorner: Vector2D  = Vector2D(0.0, 0.0)
  private val TightLowerRightCorner: Vector2D = Vector2D(20.0, 20.0)

  private val WayPointDisplacement = 5.0
  private val EpsilonDisplace      = 1e-8

  test("RayCast should return the position of the target entity when there are no obstacles"):
    val entities         = List(From, To)
    val entitiesVertexes = VertexFinder(entities)

    val result = RayCast(To, From, entities, entitiesVertexes, UpperLeftCorner, LowerRightCorner)

    result shouldBe Right(Some(To.position))

  test("RayCast should detect an obstacle hit only by a lateral ray"):
    val lateralFrom = makeMovingEntityCircle(
      id = "lateral-from",
      position = Vector2D(1.0, 1.0),
      radius = 2.0
    )
    val obstacle = makeMovingEntityRectangle(
      id = "lateral-obstacle",
      position = Vector2D(4.2928932188, 5.7071067812),
      width = 1.0,
      height = 1.0
    )
    val entities         = List(lateralFrom, To, obstacle)
    val entitiesVertexes = VertexFinder(entities.filterNot(_.id == lateralFrom.id))

    val result =
      RayCast(To, lateralFrom, entities, entitiesVertexes, UpperLeftCorner, LowerRightCorner)

    result.value.value should not be To.position

  test(
    "RayCast should return Left(RayIntersectedNothing) when neither target entity is not intersected"
  ):
    val entities         = List(From, To)
    val entitiesVertexes = VertexFinder(List(From))

    val result = RayCast(To, From, entities, entitiesVertexes, UpperLeftCorner, LowerRightCorner)

    result shouldBe Left(RayIntersectedNothing(From.id.value, To.id.value))

  test(
    "RayCast should return Left(RayIntersectedAMissingEntity) when the target entity is not in the entities list"
  ):
    val obstacle = makeMovingEntityRectangle(
      id = "obstacle",
      position = Vector2D(5.0, 5.0),
      width = 2.0,
      height = 2.0
    )

    val entities         = List(From, To)
    val entitiesVertexes = VertexFinder(List(From, To, obstacle))

    val result = RayCast(To, From, entities, entitiesVertexes, UpperLeftCorner, LowerRightCorner)

    result shouldBe Left(RayIntersectedAMissingEntity(obstacle.id.value))

  test(
    "RayCast should return None when an obstacle is intersected and no valid waypoint can be found"
  ):
    val obstacle = makeMovingEntityRectangle(
      id = "obstacle",
      position = Vector2D(5.0, 5.0),
      width = 2.0,
      height = 19.0
    )

    val entities         = List(From, To, obstacle)
    val entitiesVertexes = VertexFinder(entities)

    val result = RayCast(
      To,
      From,
      entities,
      entitiesVertexes,
      TightUpperLeftCorner,
      TightLowerRightCorner
    )

    result shouldBe Right(None)

  test(
    "RayCast should return a valid waypoint when an obstacle is intersected and a valid waypoint can be found"
  ):
    val obstacle = makeMovingEntityRectangle(
      id = "obstacle",
      position = Vector2D(5.0, 5.0),
      width = 2.0,
      height = 3.0
    )

    val entities         = List(From, To, obstacle)
    val entitiesVertexes = VertexFinder(entities.filterNot(_.id == From.id))

    val waypoints = WaypointFinder(From, obstacle)

    val expectedRawWaypoint = waypoints.minBy(_.euclideanDistance(To.position))

    val expectedWaypoint = rawToActualWaypoint(expectedRawWaypoint, obstacle)

    val result =
      RayCast(To, From, entities, entitiesVertexes, UpperLeftCorner, LowerRightCorner).value.value

    RayIntersection(
      rayStart = From.position,
      rayDirection = (To.position - From.position).normalized,
      vertexMap = entitiesVertexes
    ).value shouldBe obstacle.id

    result.x shouldBe expectedWaypoint.x +- EpsilonDisplace
    result.y shouldBe expectedWaypoint.y +- EpsilonDisplace

  test(
    "RayCast should return any valid waypoint when an obstacle is intersected and equivalent waypoints can be found"
  ):
    val obstacle = makeMovingEntityCircle(
      id = "obstacle",
      position = Vector2D(5.0, 5.0),
      radius = 1.5
    )

    val entities         = List(From, To, obstacle)
    val entitiesVertexes = VertexFinder(entities.filterNot(_.id == From.id))

    val waypoints = WaypointFinder(From, obstacle)

    val expectedWaypoints = waypoints.map(w => rawToActualWaypoint(w, obstacle)).take(2)

    val result =
      RayCast(To, From, entities, entitiesVertexes, UpperLeftCorner, LowerRightCorner).value.value

    RayIntersection(
      rayStart = From.position,
      rayDirection = (To.position - From.position).normalized,
      vertexMap = entitiesVertexes
    ).value shouldBe obstacle.id

    def sameWaypoint(a: Vector2D, b: Vector2D): Boolean =
      math.abs(a.x - b.x) <= EpsilonDisplace &&
        math.abs(a.y - b.y) <= EpsilonDisplace

    sameWaypoint(result, expectedWaypoints.head)
    || sameWaypoint(result, expectedWaypoints(1)) shouldBe true

  test("hunter radius should contain the whole hunter shape"):
    val hunter = makeMovingEntityRectangle(
      id = "radius-hunter",
      width = 10.0,
      height = 2.0
    )

    RayCast.hunterRadius(hunter) shouldBe math.sqrt(26.0)

  test("inflating circle vertexes should add the hunter clearance"):
    val obstacle = makeMovingEntityCircle(
      id = "inflated-circle",
      position = Vector2D(5.0, 5.0),
      radius = 2.0
    )
    val originalVertexes = VertexFinder(List(obstacle))

    val inflated = RayCast.inflateAllVertexes(
      originalVertexes,
      List(obstacle),
      inflation = 3.0
    )(obstacle.id)

    inflated.foreach { vertex =>
      vertex.euclideanDistance(obstacle.position) shouldBe 5.0 +- EpsilonDisplace
    }

  test("inflating a rotated rectangle should preserve its local axes"):
    val obstacle = makeMovingEntityRectangle(
      id = "rotated-obstacle",
      position = Vector2D(10.0, 10.0),
      width = 4.0,
      height = 2.0,
      rotation = 45.0
    )
    val original = VertexFinder(List(obstacle))

    val inflated =
      RayCast.inflateAllVertexes(original, List(obstacle), inflation = 1.0)(obstacle.id)

    inflated.foreach { vertex =>
      val local = (vertex - obstacle.position).rotated(-obstacle.rotation)
      math.abs(local.x) shouldBe 3.0 +- EpsilonDisplace
      math.abs(local.y) shouldBe 2.0 +- EpsilonDisplace
    }

  test("a waypoint should clear both axes of a non-square rectangle"):
    val obstacle = makeMovingEntityRectangle(
      id = "wide-obstacle",
      position = Vector2D(10.0, 10.0),
      width = 12.0,
      height = 4.0
    )
    val topRightCorner = Vector2D(16.0, 12.0)

    RayCast.actualWaypoint(obstacle, From, topRightCorner) shouldBe Vector2D(22.0, 18.0)

  test("a waypoint touching the left scene boundary should be valid"):
    RayCast.isValidWayPoint(
      To,
      From,
      Vector2D(10.0, 10.0),
      Vector2D(9.0, 0.0),
      Vector2D(20.0, 20.0)
    ) shouldBe true

  test("a waypoint touching the top scene boundary should be valid"):
    RayCast.isValidWayPoint(
      To,
      From,
      Vector2D(10.0, 10.0),
      Vector2D(0.0, 9.0),
      Vector2D(20.0, 20.0)
    ) shouldBe true

  test("a waypoint touching the right scene boundary should be valid"):
    RayCast.isValidWayPoint(
      To,
      From,
      Vector2D(10.0, 10.0),
      Vector2D(0.0, 0.0),
      Vector2D(11.0, 20.0)
    ) shouldBe true

  test("a waypoint touching the bottom scene boundary should be valid"):
    RayCast.isValidWayPoint(
      To,
      From,
      Vector2D(10.0, 10.0),
      Vector2D(0.0, 0.0),
      Vector2D(20.0, 11.0)
    ) shouldBe true

  test("a waypoint inside the left boundary but outside the top boundary should be invalid"):
    RayCast.isValidWayPoint(
      To,
      From,
      Vector2D(10.0, 0.0),
      Vector2D(0.0, 0.0),
      Vector2D(20.0, 20.0)
    ) shouldBe false
