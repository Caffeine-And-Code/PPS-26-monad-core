package integrations.monad_core.simulator.presentation.painters

import helpers.arrangers.EngineColorArranger
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{ScalaFxInit, SnapshotTesting}
import monad_core.engine.model.*
import monad_core.engine.simulator.DrawCommand
import monad_core.simulator.infrastructure.engine.painters.PaintArchitect
import monad_core.simulator.presentation.components.ResizableCanvas
import monad_core.simulator.presentation.painters.ShapePainter
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.canvas.Canvas

class ShapePainterTest extends AnyFunSuite with ScalaFxInit with Matchers with SnapshotTesting:

  val BaseEngineColor: EngineColor = EngineColorArranger.arrangeRed()

  val BaseCircleEntity: Entity = Entity.circle("EntityCircleId", Vector2D(400, 400), 50).value

  val BaseRectangleEntity: Entity =
    Entity.rectangle("EntityRectangleId", Vector2D(400, 400), 90, 150).value

  val BaseCircleSurface: Surface = Surface.circle("SurfaceCircleId", Vector2D(400, 400), 50).value

  val BaseRectangleSurface: Surface =
    Surface.rectangle("SurfaceRectangleId", Vector2D(400, 400), 150, 250).value

  val canvas: Canvas = ResizableCanvas()
  canvas.width = 800.0
  canvas.height = 800.0

  def circleCommand(drawable: Locatable, color: EngineColor = BaseEngineColor): DrawCommand =
    PaintArchitect.drawCircle(drawable, color).value

  def rectangleCommand(drawable: Locatable, color: EngineColor = BaseEngineColor): DrawCommand =
    PaintArchitect.drawRectangle(drawable, color).value

  test("paint processes an immutable sequence of commands"):
    val commands = Vector(
      circleCommand(BaseCircleEntity),
      rectangleCommand(BaseRectangleEntity),
      circleCommand(BaseCircleSurface),
      rectangleCommand(BaseRectangleSurface)
    )

    onFxThread:
      ShapePainter.paint(canvas.graphicsContext2D, commands)

  test("paint draws the Circle Commands that contains a Circle Entity"):
    onFxThread:
      ShapePainter.paint(canvas.graphicsContext2D, Vector(circleCommand(BaseCircleEntity)))

    assertMatchesVisualSnapshot("circle_entity_flush_result", canvas, maxDiffPercentage = 2.0)

  test("paint draws the Rectangle Commands that contains a Rectangle Entity"):
    onFxThread:
      ShapePainter.paint(canvas.graphicsContext2D, Vector(rectangleCommand(BaseRectangleEntity)))

    assertMatchesVisualSnapshot("rectangle_entity_flush_result", canvas, maxDiffPercentage = 3.0)

  test(
    "paint draws the Circle Commands that contains a Circle Entity with the corresponding Team Color"
  ):
    val entityWithATeam = BaseCircleEntity.withTeamId(Some("TestTeam")).value
    val teamColor       = PaintArchitect.teamIdColorRelation(entityWithATeam.teamId.get).value

    onFxThread:
      ShapePainter.paint(
        canvas.graphicsContext2D,
        Vector(circleCommand(entityWithATeam, teamColor))
      )

    assertMatchesVisualSnapshot(
      "circle_entity_with_team_flush_result",
      canvas,
      maxDiffPercentage = 2.0
    )

  test(
    "paint draws the Rectangle Commands that contains a Rectangle Entity with the corresponding Team Color"
  ):
    val entityWithATeam = BaseRectangleEntity.withTeamId(Some("TestTeam")).value
    val teamColor       = PaintArchitect.teamIdColorRelation(entityWithATeam.teamId.get).value

    onFxThread:
      ShapePainter.paint(
        canvas.graphicsContext2D,
        Vector(rectangleCommand(entityWithATeam, teamColor))
      )

    assertMatchesVisualSnapshot(
      "rectangle_entity_with_team_flush_result",
      canvas,
      maxDiffPercentage = 3.0
    )

  test("paint draws the Circle Commands that contains a Circle Surface"):
    onFxThread:
      ShapePainter.paint(canvas.graphicsContext2D, Vector(circleCommand(BaseCircleSurface)))

    assertMatchesVisualSnapshot("circle_surface_flush_result", canvas, maxDiffPercentage = 2.0)

  test("paint draws the Rectangle Commands that contains a Rectangle Surface"):
    onFxThread:
      ShapePainter.paint(canvas.graphicsContext2D, Vector(rectangleCommand(BaseRectangleSurface)))

    assertMatchesVisualSnapshot("rectangle_surface_flush_result", canvas, maxDiffPercentage = 6.0)

  test("paint should process explicitly provided commands"):
    val testCanvas = Canvas(800, 800)
    val commands = Vector(
      DrawCommand.Circle(100.0, 100.0, 25.0, BaseEngineColor),
      DrawCommand.Rectangle(200.0, 200.0, 50.0, 80.0, 30.0, BaseEngineColor)
    )

    ShapePainter.paint(testCanvas.graphicsContext2D, commands)

  test("paint gracefully handles an empty command sequence"):
    val testCanvas = Canvas(800, 800)

    ShapePainter.paint(testCanvas.graphicsContext2D, Vector.empty)
