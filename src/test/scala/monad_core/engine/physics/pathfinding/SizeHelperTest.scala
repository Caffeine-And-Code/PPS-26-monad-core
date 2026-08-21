package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Entity, Vector2D}
import CircleVertexes.*
import RectangleVertexes.*
import SizeHelper.*
import monad_core.engine.helper.DummyEntityHelper.{makeFixedEntityCircle, makeFixedEntityRectangle}
import monad_core.engine.physics.pathfinding.CircleVertexes.*
import monad_core.engine.physics.pathfinding.RectangleVertexes.*
import monad_core.engine.physics.pathfinding.SizeHelper.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SizeHelperTest extends AnyFunSuite with Matchers:

  private val Epsilon = 1e-9
  
  private val EntityRectangle = makeFixedEntityRectangle(
    position = Vector2D(5.0, 6.0),
    height = 2.0,
    width = 3.0
  )

  private val EntityCircle = makeFixedEntityCircle(
    position = Vector2D(5.0, 6.0),
    radius = 2.0
  )

  private def expectedVSize(entity: Entity, rotation: Double = 0.0): Double =
    entity.shape match
      case rectangle: Rectangle =>
        rectangle.verticalSize(entity.position, rotation)
      case circle: Circle =>
        circle.verticalSize()

  private def expectedHSize(entity: Entity, rotation: Double = 0.0): Double =
    entity.shape match
      case rectangle: Rectangle =>
        rectangle.horizontalSize(entity.position, rotation)
      case circle: Circle =>
        circle.horizontalSize()

  test("this extension should find the vertical size of a rectangle"):

    val expectedVerticalSize = expectedVSize(EntityRectangle)

    val actualVerticalSize = verticalShapeSize(EntityRectangle)

    actualVerticalSize shouldBe expectedVerticalSize

  test("this extension should find the vertical size of a circle"):

    val expectedVerticalSize = expectedVSize(EntityCircle)

    val actualVerticalSize = verticalShapeSize(EntityCircle)

    actualVerticalSize shouldBe expectedVerticalSize

  test("this extension should find the horizontal size of a rectangle"):

    val expectedHorizontalSize = expectedHSize(EntityRectangle)

    val actualHorizontalSize = horizontalShapeSize(EntityRectangle)

    actualHorizontalSize shouldBe expectedHorizontalSize

  test("this extension should find the horizontal size of a circle"):

    val expectedHorizontalSize = expectedHSize(EntityCircle)

    val actualHorizontalSize = horizontalShapeSize(EntityCircle)

    actualHorizontalSize shouldBe expectedHorizontalSize

  test("shape sizes should include rectangle rotation"):
    val rotated            = EntityRectangle.rotateTo(30.0).value
    val expectedHorizontalSize            = expectedHSize(rotated, 30.0)
    val expectedVerticalSize              = expectedVSize(rotated, 30.0)

    horizontalShapeSize(rotated) shouldBe expectedHorizontalSize +- Epsilon
    verticalShapeSize(rotated) shouldBe expectedVerticalSize +- Epsilon
