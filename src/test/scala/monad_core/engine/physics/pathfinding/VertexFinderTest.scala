package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Entity, Shape2D, Vector2D}
import PathCircle.vertexes
import PathRectangle.vertexes
import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeFixedEntityRectangle}
import monad_core.engine.physics.helper.PhysicsEntityHelper.*
import monad_core.engine.physics.pathfinding.PathCircle.vertexes
import monad_core.engine.physics.pathfinding.PathRectangle.vertexes
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class VertexFinderTest extends AnyFunSuite with Matchers:

  private val CircleVertexesNumber = 16

  test("the VertexFinder should return an empty map when the entities list is empty"):
    val entities = List.empty

    val result = VertexFinder(entities)

    result shouldBe Map.empty

  test("the VertexFinder should return a map with vertexes for each entity"):
    val circleEntity    = makeFixedEntityCircle("circle", Vector2D(0, 0), 1.0)
    val rectangleEntity = makeFixedEntityRectangle("rectangle", Vector2D(1, 1), 2.0, 3.0)

    val entities = List(circleEntity, rectangleEntity)

    val expectedMap = entities
      .map(entity =>
        entity.id -> (entity.shape match
          case circle: Circle       => circle.vertexes(entity.position, CircleVertexesNumber)
          case rectangle: Rectangle => rectangle.vertexes(entity.position)
        )
      )
      .toMap

    val result = VertexFinder(entities)

    result should contain key circleEntity.id
    result should contain key rectangleEntity.id

    val circleVertexes    = result(circleEntity.id)
    val rectangleVertexes = result(rectangleEntity.id)

    circleVertexes should contain theSameElementsAs expectedMap(circleEntity.id)
    rectangleVertexes should contain theSameElementsAs expectedMap(rectangleEntity.id)
