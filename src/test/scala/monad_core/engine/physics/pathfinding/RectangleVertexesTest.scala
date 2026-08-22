package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Rectangle
import monad_core.engine.model.{+, Shape2D, Vector2D, rotated}
import monad_core.engine.physics.pathfinding.RectangleVertexes.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RectangleVertexesTest extends AnyFunSuite with Matchers:

  private val Shape: Rectangle = Shape2D.rectangle(length = 4.0, height = 3.0).value
  private val Position         = Vector2D(4.0, 5.0)
  private val Rotation         = 30.0
  private val Epsilon          = 1e-9

  private def assertVector(actual: Vector2D, expected: Vector2D): Unit =
    actual.x shouldBe expected.x +- Epsilon
    actual.y shouldBe expected.y +- Epsilon

  test(
    "this extension should generate correct vertexes for a given rectangle, its center, and number of vertexes"
  ):

    val expectedVertexes = List(
      Vector2D(-Shape.halfLength, -Shape.halfHeight),
      Vector2D(Shape.halfLength, -Shape.halfHeight),
      Vector2D(Shape.halfLength, Shape.halfHeight),
      Vector2D(-Shape.halfLength, Shape.halfHeight)
    ).map(localVertex => Position + localVertex.rotated(Rotation))

    val actualVertexes = Shape.vertexes(Position, Rotation)

    actualVertexes should have size expectedVertexes.size
    actualVertexes
      .zip(expectedVertexes)
      .foreach((actual, expected) => assertVector(actual, expected))

  test("this extension should find the upper vertex of a rectangle"):

    val expectedUpperVertex = Shape.vertexes(Position, Rotation).minBy(_.y)

    val actualUpperVertex = Shape.upperVertex(Position, Rotation)

    actualUpperVertex shouldBe expectedUpperVertex

  test("this extension should find the lower vertex of a rectangle"):

    val expectedLowerVertex = Shape.vertexes(Position, Rotation).maxBy(_.y)

    val actualLowerVertex = Shape.lowerVertex(Position, Rotation)

    actualLowerVertex shouldBe expectedLowerVertex

  test("this extension should find the left vertex of a rectangle"):

    val expectedLeftVertex = Shape.vertexes(Position, Rotation).minBy(_.x)

    val actualLeftVertex = Shape.leftVertex(Position, Rotation)

    actualLeftVertex shouldBe expectedLeftVertex

  test("this extension should find the right vertex of a rectangle"):

    val expectedRightVertex = Shape.vertexes(Position, Rotation).maxBy(_.x)

    val actualRightVertex = Shape.rightVertex(Position, Rotation)

    actualRightVertex shouldBe expectedRightVertex

  test("this extension should find the horizontal size of a rectangle"):

    val leftVertex             = Shape.leftVertex(Position, Rotation)
    val rightVertex            = Shape.rightVertex(Position, Rotation)
    val expectedHorizontalSize = rightVertex.x - leftVertex.x

    val actualHorizontalSize = Shape.horizontalSize(Position, Rotation)

    actualHorizontalSize shouldBe expectedHorizontalSize

  test("this extension should find the vertical size of a rectangle"):

    val upperVertex          = Shape.upperVertex(Position, Rotation)
    val lowerVertex          = Shape.lowerVertex(Position, Rotation)
    val expectedVerticalSize = lowerVertex.y - upperVertex.y

    val actualVerticalSize = Shape.verticalSize(Position, Rotation)

    actualVerticalSize shouldBe expectedVerticalSize

  test("rotating a rectangle should rotate its vertexes around its center"):
    val expectedVertexes = List(
      Vector2D(5.5, 3.0),
      Vector2D(5.5, 7.0),
      Vector2D(2.5, 7.0),
      Vector2D(2.5, 3.0)
    )

    val actualVertexes = Shape.vertexes(Position, rotation = 90.0)

    actualVertexes should have size expectedVertexes.size
    actualVertexes
      .zip(expectedVertexes)
      .foreach((actual, expected) => assertVector(actual, expected))

  test("rotating a rectangle by a quarter turn should swap its sizes"):
    val horizontalSize = Shape.horizontalSize(Position, rotation = 90.0)
    val verticalSize   = Shape.verticalSize(Position, rotation = 90.0)

    horizontalSize shouldBe Shape.height +- Epsilon
    verticalSize shouldBe Shape.length +- Epsilon
