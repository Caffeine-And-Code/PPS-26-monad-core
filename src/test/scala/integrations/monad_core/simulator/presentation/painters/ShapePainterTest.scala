package integrations.monad_core.simulator.presentation.painters

import helpers.arrangers.EngineColorArranger
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{ScalaFxInit, SnapshotTesting}
import monad_core.engine.model.*
import monad_core.simulator.application.engine.{DrawCommand, ShapeArchitect}
import monad_core.simulator.infrastructure.engine.painters.PaintArchitect
import monad_core.simulator.presentation.components.ResizableCanvas
import monad_core.simulator.presentation.painters.ShapePainter
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.canvas.Canvas

class ShapePainterTest
    extends AnyFunSuite
    with ScalaFxInit
    with MockFactory
    with Matchers
    with SnapshotTesting
    with BeforeAndAfterEach:
  given ShapeArchitect = PaintArchitect

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

  override def beforeEach(): Unit =
    PaintArchitect.drainBuffer()

  override def afterEach(): Unit =
    PaintArchitect.drainBuffer()

  def enlistCircle(drawable: Locatable): Unit =
    PaintArchitect.drawCircle(drawable, BaseEngineColor)

  def enlistRectangle(drawable: Locatable): Unit =
    PaintArchitect.drawRectangle(drawable, BaseEngineColor)

  test("paint effectively drains the buffer from ShapeArchitect"):
    enlistCircle(BaseCircleEntity)
    enlistRectangle(BaseRectangleEntity)
    enlistCircle(BaseCircleSurface)
    enlistRectangle(BaseRectangleSurface)

    ShapePainter.paint(canvas.graphicsContext2D)

    PaintArchitect.drainBuffer().length should be(0)

  test("paint draws the Circle Commands that contains a Circle Entity"):
    enlistCircle(BaseCircleEntity)

    onFxThread {
      ShapePainter.paint(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("circle_entity_flush_result", canvas, maxDiffPercentage = 2.0)

  test("paint draws the Rectangle Commands that contains a Rectangle Entity"):
    enlistRectangle(BaseRectangleEntity)

    onFxThread {
      ShapePainter.paint(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("rectangle_entity_flush_result", canvas, maxDiffPercentage = 3.0)

  test(
    "paint draws the Circle Commands that contains a Circle Entity with the corresponding Team Color"
  ):
    val entityWithATeam = BaseCircleEntity.withTeamId("TestTeam").value
    PaintArchitect.drawCircle(
      entityWithATeam,
      PaintArchitect.teamIdColorRelation(entityWithATeam.teamId.get).value
    )

    onFxThread {
      ShapePainter.paint(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot(
      "circle_entity_with_team_flush_result",
      canvas,
      maxDiffPercentage = 2.0
    )

  test(
    "paint draws the Rectangle Commands that contains a Rectangle Entity with the corresponding Team Color"
  ):
    val entityWithATeam = BaseRectangleEntity.withTeamId("TestTeam").value
    PaintArchitect.drawRectangle(
      entityWithATeam,
      PaintArchitect.teamIdColorRelation(entityWithATeam.teamId.get).value
    )

    onFxThread {
      ShapePainter.paint(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot(
      "rectangle_entity_with_team_flush_result",
      canvas,
      maxDiffPercentage = 3.0
    )

  test("paint draws the Circle Commands that contains a Circle Surface"):
    enlistCircle(BaseCircleSurface)

    onFxThread {
      ShapePainter.paint(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("circle_surface_flush_result", canvas, maxDiffPercentage = 2.0)

  test("paint draws the Rectangle Commands that contains a Rectangle Surface"):
    enlistRectangle(BaseRectangleSurface)

    onFxThread {
      ShapePainter.paint(canvas.graphicsContext2D)
    }

    assertMatchesVisualSnapshot("rectangle_surface_flush_result", canvas, maxDiffPercentage = 3.0)

  test("ShapePainter.paint calls drainBuffer on ShapeArchitect and processes commands"):
    val canvas = Canvas(800, 800)
    val gc     = canvas.graphicsContext2D

    given mockArchitect: ShapeArchitect = mock[ShapeArchitect]

    val expectedCommands = List(
      DrawCommand.Circle(100.0, 100.0, 25.0, BaseEngineColor),
      DrawCommand.Rectangle(200.0, 200.0, 50.0, 80.0, 30.0, BaseEngineColor)
    )

    (() => mockArchitect.drainBuffer()).expects().returns(expectedCommands).once()

    ShapePainter.paint(gc)

  test("ShapePainter.paint gracefully handles an empty buffer"):
    val canvas = Canvas(800, 800)
    val gc     = canvas.graphicsContext2D

    given mockArchitect: ShapeArchitect = mock[ShapeArchitect]

    (() => mockArchitect.drainBuffer()).expects().returns(Nil).once()

    ShapePainter.paint(gc)
