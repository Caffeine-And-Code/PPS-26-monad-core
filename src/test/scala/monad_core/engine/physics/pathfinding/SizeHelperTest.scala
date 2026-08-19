package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.physics.helper.PhysicsEntityHelper.*
import monad_core.engine.physics.pathfinding.PathCircle.*
import monad_core.engine.physics.pathfinding.PathRectangle.*
import monad_core.engine.physics.pathfinding.SizeHelper.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SizeHelperTest extends AnyFunSuite with Matchers:

  private val EntityRectangle = makeFixedEntityRectangle(
    position = Vector2D(5.0, 6.0),
    height = 2.0,
    width = 3.0
  )

  private val EntityCircle = makeFixedEntityCircle(
    position = Vector2D(5.0, 6.0),
    radius = 2.0
  )

  private def expectedVSize(entity: Entity): Double =
    entity.shape match
      case rectangle: Rectangle =>
        rectangle.verticalSize(entity.position)
      case circle: Circle =>
        circle.verticalSize()

  private def expectedHSize(entity: Entity): Double =
    entity.shape match
      case rectangle: Rectangle =>
        rectangle.horizontalSize(entity.position)
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
