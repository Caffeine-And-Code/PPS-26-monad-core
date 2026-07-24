package integrations.monad_core.simulator.presentation.painters

import integrations.monad_core.simulator.presentation.support.{ScalaFxInit, SnapshotTesting}
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.simulator.presentation.components.ResizableCanvas
import monad_core.simulator.presentation.painters.Drawer
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

class DrawerTest extends AnyFunSuite with ScalaFxInit with MockFactory with Matchers with SnapshotTesting:
  val canvas: Canvas = ResizableCanvas()
  canvas.width = 800.0
  canvas.height = 800.0

  def enlistCircle(): Unit =
    val entity: Entity = Entity.circle("CircleId", Vector2D(400, 400), 50).value
    Drawer.drawCircle(entity, Color.Red)

  def enlistRectangle(): Unit =
    val entity: Entity = Entity.rectangle("RectangleId", Vector2D(400, 400), 100, 200).value
    Drawer.drawRectangle(entity, Color.Red)

  test("flush effectively clears the buffer"):
    enlistCircle()
    enlistRectangle()

    Drawer.flush(canvas.graphicsContext2D)

    Drawer.getBuffer.toList.length should be(0)

  test("flush draws the Circle Commands"):
    enlistCircle()

    runOnFxThread{
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("circle_flush_result", canvas)


  test("flush draws the Rectangle Commands"):
    enlistRectangle()

    runOnFxThread{
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("rectangle_flush_result", canvas)
