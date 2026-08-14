package monad_core.engine.physics.pathfinding

import monad_core.engine.model.{LocatableId, Vector2D}
import monad_core.engine.physics.helper.PhysicsEntityHelper.{makeFixedEntityCircle, makeFixedEntityRectangle}
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RayIntersectionTest extends AnyFunSuite with Matchers:
  private val RayStart = Vector2D(1.0, 1.0)
  private val RayDirection = Vector2D(1.0, 0.0)

  private val IntersectedCircleEntity = makeFixedEntityCircle(
    id = "circle",
    position = Vector2D(10.0, 1.0),
    radius = 0.5
  )

  private val IntersectedRectangleEntity = makeFixedEntityRectangle(
    id = "rectangle",
    position = Vector2D(3.0, 1.0),
    height = 1.0,
    width = 1.0
  )

  test("RayIntersection should return None if vertexes are empty") :
    val vertexMap = Map.empty[LocatableId, List[Vector2D]]

    val result = RayIntersection(RayStart, RayDirection, vertexMap)

    result shouldBe None

  test("RayIntersection should return None if ray does not intersect any vertexes") :

    val vertexMap = VertexFinder(
      List(
        makeFixedEntityRectangle(
          position = Vector2D(5.0, 5.0),
          height = 1.0,
          width = 1.0
        )
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap)

    result shouldBe None

  test("RayIntersection should return the id of an intersected rectangle") :

    val vertexMap = VertexFinder(
      List(
        IntersectedRectangleEntity
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap).value

    result shouldBe IntersectedRectangleEntity.id

  test("RayIntersection should return the id of an intersected circle"):

    val vertexMap = VertexFinder(
      List(
        IntersectedCircleEntity
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap).value

    result shouldBe IntersectedCircleEntity.id

  test("RayIntersection should return the id of the closest intersected entity") :

    val vertexMap = VertexFinder(
      List(
        IntersectedCircleEntity,
        IntersectedRectangleEntity
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap).value

    result shouldBe IntersectedRectangleEntity.id

  test("RayIntersection should return none if the ray is parallel to the edges of the entity") :

    val vertexMap = VertexFinder(
      List(
        makeFixedEntityRectangle(
          position = Vector2D(1.0, 2.0),
          height = 1.0,
          width = 1.0
        )
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap)

    result shouldBe None

  test("RayIntersection should return none if the ray is pointing away from the entity") :

    val vertexMap = VertexFinder(
      List(
        makeFixedEntityRectangle(
          position = Vector2D(0.0, 1.0),
          height = 1.0,
          width = 1.0
        )
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap)

    result shouldBe None