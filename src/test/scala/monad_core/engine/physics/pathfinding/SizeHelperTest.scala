package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.Vector2D
import monad_core.engine.physics.helper.PhysicsEntityHelper.*
import PathCircle.*
import PathRectangle.*
import SizeHelper.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SizeHelperTest extends AnyFunSuite with Matchers :

  private val EntityRectangle = makeFixedEntityRectangle(
    position = Vector2D(5.0, 6.0),
    height = 2.0,
    width = 3.0
  )

  private val EntityCircle = makeFixedEntityCircle(
    position = Vector2D(5.0, 6.0),
    radius = 2.0
  )

  test("this extension should find the vertical size of a rectangle") :

    val expectedVerticalSize = EntityRectangle
      .shape.asInstanceOf[Rectangle]
      .verticalSize(EntityRectangle.position)

    val actualVerticalSize = verticalShapeSize(EntityRectangle)

    actualVerticalSize shouldBe expectedVerticalSize

  test("this extension should find the vertical size of a circle"):

    val expectedVerticalSize = EntityCircle
      .shape.asInstanceOf[Circle]
      .verticalSize()

    val actualVerticalSize = verticalShapeSize(EntityCircle)

    actualVerticalSize shouldBe expectedVerticalSize

  test("this extension should find the horizontal size of a rectangle"):

    val expectedHorizontalSize = EntityRectangle
      .shape.asInstanceOf[Rectangle]
      .horizontalSize(EntityRectangle.position)

    val actualHorizontalSize = horizontalShapeSize(EntityRectangle)

    actualHorizontalSize shouldBe expectedHorizontalSize

  test("this extension should find the horizontal size of a circle"):

    val expectedHorizontalSize = EntityCircle
      .shape.asInstanceOf[Circle]
      .horizontalSize()

    val actualHorizontalSize = horizontalShapeSize(EntityCircle)

    actualHorizontalSize shouldBe expectedHorizontalSize