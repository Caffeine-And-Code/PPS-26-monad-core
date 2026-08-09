package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.Circle
import monad_core.engine.model.Vector2D
import PathCircle.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PathCircleTest extends AnyFunSuite with Matchers :
  
  val Shape: Circle = Circle(radius = 2.0) 
  
  test("this extension should generate correct vertexes for a given circle, its center, and number of vertexes") :

    val position = Vector2D(4.0, 5.0)
    val numberOfVertexes = 10

    val expectedVertexes = (0 until numberOfVertexes).map { i =>
      val angle = 2 * math.Pi * i / numberOfVertexes
      PointOnCircle(position, Shape.radius, angle)
    }.toList

    val actualVertexes = Shape.vertexes(position, numberOfVertexes)

    actualVertexes should contain theSameElementsAs expectedVertexes
    
  test("this extension should return the correct vertical size for a given circle") :

    val expectedVerticalSize = Shape.radius * 2
    
    val actualVerticalSize = Shape.verticalSize()

    actualVerticalSize shouldBe expectedVerticalSize
    
  test("this extension should return the correct horizontal size for a given circle") :
    val expectedHorizontalSize = Shape.radius * 2
    
    val actualHorizontalSize = Shape.horizontalSize()

    actualHorizontalSize shouldBe expectedHorizontalSize