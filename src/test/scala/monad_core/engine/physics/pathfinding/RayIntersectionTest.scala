package monad_core.engine.physics.pathfinding

import monad_core.engine.model.{LocatableId, Vector2D}
import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeFixedEntityRectangle}
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RayIntersectionTest extends AnyFunSuite with Matchers:
  private val RayStart     = Vector2D(1.0, 1.0)
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

  test("RayIntersection should return None if vertexes are empty"):
    val vertexMap = Map.empty[LocatableId, List[Vector2D]]

    val result = RayIntersection(RayStart, RayDirection, vertexMap)

    result shouldBe None

  test("RayIntersection should return None if ray does not intersect any vertexes"):

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

  test("RayIntersection should return the id of an intersected rectangle"):

    val vertexMap = VertexFinder(
      List(
        IntersectedRectangleEntity
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap).value

    result shouldBe IntersectedRectangleEntity.id

  test("RayIntersection should detect a vertex lying exactly on the ray"):
    val vertexEntity = makeFixedEntityRectangle(
      id = "vertex",
      position = Vector2D(5.0, 1.0),
      height = 1.0,
      width = 1.0
    )
    val vertexMap = Map(vertexEntity.id -> List(Vector2D(5.0, 1.0)))

    val result = RayIntersection(RayStart, RayDirection, vertexMap)

    result shouldBe Some(vertexEntity.id)

  test("RayIntersection should return the distance of the closest hit"):
    val vertexEntity = makeFixedEntityRectangle(
      id = "vertex-distance",
      position = Vector2D(5.0, 1.0),
      height = 1.0,
      width = 1.0
    )
    val vertexMap = Map(vertexEntity.id -> List(Vector2D(5.0, 1.0)))

    val result = RayIntersection.withDistance(RayStart, RayDirection, vertexMap)

    result shouldBe Some(vertexEntity.id -> 4.0)

  test("RayIntersection should return the id of an intersected circle"):

    val vertexMap = VertexFinder(
      List(
        IntersectedCircleEntity
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap).value

    result shouldBe IntersectedCircleEntity.id

  test("RayIntersection should return the id of the closest intersected entity"):

    val vertexMap = VertexFinder(
      List(
        IntersectedCircleEntity,
        IntersectedRectangleEntity
      )
    )

    val result = RayIntersection(RayStart, RayDirection, vertexMap).value

    result shouldBe IntersectedRectangleEntity.id

  test("RayIntersection should return none if the ray is parallel to the edges of the entity"):

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

  test("RayIntersection should return none if the ray is pointing away from the entity"):

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

  test("RayIntersection should ignore a vertex exactly at the epsilon distance"):
    val vertexMap = Map(
      LocatableId("epsilon").value -> List(Vector2D(RayStart.x + 1e-8, RayStart.y))
    )

    RayIntersection.withDistance(RayStart, RayDirection, vertexMap) shouldBe None

  test("RayIntersection should ignore a zero-length ray direction"):
    val vertexMap = Map(
      LocatableId("vertex").value -> List(Vector2D(2.0, 1.0))
    )

    RayIntersection(RayStart, Vector2D(0.0, 0.0), vertexMap) shouldBe None

  test("RayIntersection should include a vertex exactly at the perpendicular epsilon"):
    val vertexMap = Map(
      LocatableId("epsilon-perpendicular").value ->
        List(Vector2D(RayStart.x + 4.0, RayStart.y + 1e-8))
    )

    RayIntersection.withDistance(RayStart, RayDirection, vertexMap) shouldBe defined

  test("raySegmentIntersection should include an intersection at the ray origin"):
    RayIntersection.raySegmentIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      Vector2D(0.0, -1.0),
      Vector2D(0.0, 1.0)
    ) shouldBe Some(0.0)

  test("raySegmentIntersection should include the first segment endpoint"):
    RayIntersection.raySegmentIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      Vector2D(5.0, 0.0),
      Vector2D(5.0, 1.0)
    ) shouldBe Some(5.0)

  test("raySegmentIntersection should include the second segment endpoint"):
    RayIntersection.raySegmentIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      Vector2D(5.0, -1.0),
      Vector2D(5.0, 0.0)
    ) shouldBe Some(5.0)

  test("raySegmentIntersection should process a cross product exactly at epsilon"):
    val halfEpsilon = RayIntersection.Epsilon / 2.0

    RayIntersection.raySegmentIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      Vector2D(5.0, -halfEpsilon),
      Vector2D(5.0, halfEpsilon)
    ) shouldBe Some(5.0)

  test("raySegmentIntersection should reject a non-zero cross product below epsilon"):
    val quarterEpsilon = RayIntersection.Epsilon / 4.0

    RayIntersection.raySegmentIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      Vector2D(5.0, -quarterEpsilon),
      Vector2D(5.0, quarterEpsilon)
    ) shouldBe None

  test("withDistance should exclude a segment hit exactly at epsilon"):
    val id = LocatableId("epsilon-segment").value
    val vertexMap = Map(
      id -> List(
        Vector2D(RayIntersection.Epsilon, -1.0),
        Vector2D(RayIntersection.Epsilon, 1.0)
      )
    )

    RayIntersection.withDistance(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      vertexMap
    ) shouldBe None

  test("rayVertexIntersection should exclude a vertex exactly at the forward epsilon"):
    RayIntersection.rayVertexIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      Vector2D(RayIntersection.Epsilon, 0.0)
    ) shouldBe None

  test("rayVertexIntersection should include a vertex exactly at the lateral epsilon"):
    RayIntersection.rayVertexIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0),
      Vector2D(5.0, RayIntersection.Epsilon)
    ) shouldBe Some(5.0)

  test("rayVertexIntersection should reject a zero direction"):
    RayIntersection.rayVertexIntersection(
      Vector2D(0.0, 0.0),
      Vector2D(0.0, 0.0),
      Vector2D(1.0, 0.0)
    ) shouldBe None
