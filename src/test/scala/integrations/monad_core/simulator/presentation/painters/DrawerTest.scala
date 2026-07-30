package integrations.monad_core.simulator.presentation.painters

import integrations.monad_core.simulator.presentation.support.{ScalaFxInit, SnapshotTesting}
import monad_core.engine.model.{Entity, Locatable, Surface, Vector2D}
import monad_core.simulator.presentation.components.ResizableCanvas
import monad_core.simulator.presentation.painters.Drawer
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

class DrawerTest extends AnyFunSuite with ScalaFxInit with MockFactory with Matchers with SnapshotTesting with BeforeAndAfterEach:
  val BaseCircleEntity: Entity = Entity.circle("EntityCircleId", Vector2D(400, 400), 50).value
  val BaseRectangleEntity: Entity = Entity.rectangle("EntityRectangleId", Vector2D(400, 400), 90, 150).value
  val BaseCircleSurface: Surface = Surface.circle("SurfaceCircleId", Vector2D(400, 400), 50).value
  val BaseRectangleSurface: Surface = Surface.rectangle("SurfaceRectangleId", Vector2D(400, 400), 150, 250).value
  val canvas: Canvas = ResizableCanvas()
  canvas.width = 800.0
  canvas.height = 800.0

  override def beforeEach(): Unit =
    Drawer.getBuffer.clear()

  def enlistCircle(drawable: Locatable): Unit =
    Drawer.drawCircle(drawable, Color.Red)

  def enlistRectangle(drawable: Locatable): Unit =
    Drawer.drawRectangle(drawable, Color.Red)

  test("flush effectively clears the buffer"):
    enlistCircle(BaseCircleEntity)
    enlistRectangle(BaseRectangleEntity)
    enlistCircle(BaseCircleSurface)
    enlistRectangle(BaseRectangleSurface)

    Drawer.flush(canvas.graphicsContext2D)

    Drawer.getBuffer.toList.length should be(0)

  test("flush draws the Circle Commands that contains a Circle Entity"):
    enlistCircle(BaseCircleEntity)

    runOnFxThread {
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("circle_entity_flush_result", canvas, maxDiffPercentage = 2.0)

  test("flush draws the Rectangle Commands that contains a Rectangle Entity"):
    enlistRectangle(BaseRectangleEntity)

    runOnFxThread {
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("rectangle_entity_flush_result", canvas, maxDiffPercentage = 3.0)

  test("flush draws the Circle Commands that contains a Circle Entity with the corresponding Team Color"):
    val entityWithATeam = BaseCircleEntity.withTeamId("TestTeam").value
    Drawer.drawCircle(entityWithATeam, Drawer.teamIdColorRelation(entityWithATeam.teamId.get))

    runOnFxThread {
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("circle_entity_with_team_flush_result", canvas, maxDiffPercentage = 2.0)

  test("flush draws the Rectangle Commands that contains a Rectangle Entity with the corresponding Team Color"):
    val entityWithATeam = BaseRectangleEntity.withTeamId("TestTeam").value
    Drawer.drawRectangle(entityWithATeam, Drawer.teamIdColorRelation(entityWithATeam.teamId.get))

    runOnFxThread {
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("rectangle_entity_with_team_flush_result", canvas, maxDiffPercentage = 3.0)

  test("flush draws the Circle Commands that contains a Circle Surface"):
    enlistCircle(BaseCircleSurface)

    runOnFxThread {
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("circle_surface_flush_result", canvas, maxDiffPercentage = 2.0)

  test("flush draws the Rectangle Commands that contains a Rectangle Surface"):
    enlistRectangle(BaseRectangleSurface)

    runOnFxThread {
      Drawer.flush(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("rectangle_surface_flush_result", canvas, maxDiffPercentage = 3.0)