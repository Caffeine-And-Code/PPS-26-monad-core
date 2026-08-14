package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Rectangle
import monad_core.engine.model.{Shape2D, Vector2D}
import PathRectangle.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PathRectangleTest extends AnyFunSuite with Matchers :

  val Shape: Rectangle = Shape2D.rectangle(length = 4.0, height = 3.0).value
  val Position = Vector2D(4.0, 5.0)

  test("this extension should generate correct vertexes for a given rectangle, its center, and number of vertexes") :

    val expectedVertexes = List(
      Vector2D(
        Position.x - Shape.halfLength,
        Position.y - Shape.halfHeight
      ),
      Vector2D(
        Position.x + Shape.halfLength,
        Position.y - Shape.halfHeight
      ),
      Vector2D(
        Position.x + Shape.halfLength,
        Position.y + Shape.halfHeight
      ),
      Vector2D(
        Position.x - Shape.halfLength,
        Position.y + Shape.halfHeight
      )
    )

    val actualVertexes = Shape.vertexes(Position)

    actualVertexes should contain theSameElementsAs expectedVertexes

  test("this extension should find the upper vertex of a rectangle") :

    val expectedUpperVertex = Shape.vertexes(Position).minBy(_.y)
    
    val actualUpperVertex = Shape.upperVertex(Position)

    actualUpperVertex shouldBe expectedUpperVertex

  test("this extension should find the lower vertex of a rectangle") :

    val expectedLowerVertex = Shape.vertexes(Position).maxBy(_.y)

    val actualLowerVertex = Shape.lowerVertex(Position)

    actualLowerVertex shouldBe expectedLowerVertex

  test("this extension should find the left vertex of a rectangle") :

    val expectedLeftVertex = Shape.vertexes(Position).minBy(_.x)

    val actualLeftVertex = Shape.leftVertex(Position)

    actualLeftVertex shouldBe expectedLeftVertex

  test("this extension should find the right vertex of a rectangle") :

    val expectedRightVertex = Shape.vertexes(Position).maxBy(_.x)

    val actualRightVertex = Shape.rightVertex(Position)

    actualRightVertex shouldBe expectedRightVertex
    
  test("this extension should find the horizontal size of a rectangle") :

    val leftVertex = Shape.leftVertex(Position)
    val rightVertex = Shape.rightVertex(Position)
    val expectedHorizontalSize = rightVertex.x - leftVertex.x

    val actualHorizontalSize = Shape.horizontalSize(Position)

    actualHorizontalSize shouldBe expectedHorizontalSize
    
  test("this extension should find the vertical size of a rectangle") :
    
    val upperVertex = Shape.upperVertex(Position)
    val lowerVertex = Shape.lowerVertex(Position)
    val expectedVerticalSize = lowerVertex.y - upperVertex.y

    val actualVerticalSize = Shape.verticalSize(Position)

    actualVerticalSize shouldBe expectedVerticalSize
    
  
