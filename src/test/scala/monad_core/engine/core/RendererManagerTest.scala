package monad_core.engine.core

import helpers.arrangers.EngineColorArranger
import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.simulator.{DrawCommand, Painter, RendererManager}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RendererManagerTest extends AnyFunSuite with Matchers with MockFactory:

  private val ZeroVector = Vector2D(0.0, 0.0)

  test("renderer manager should draw surfaces using the base color"):
    given painter: Painter = mock[Painter]

    val entityColor      = EngineColorArranger.arrangeWhite()
    val surfaceColor     = EngineColorArranger.arrangeRed()
    val circleSurface    = Surface.circle("s1", ZeroVector, 10.0).value
    val rectSurface      = Surface.rectangle("s2", ZeroVector, 20.0, 30.0).value
    val mockState        = mock[State]
    val circleCommand    = DrawCommand.Circle(0, 0, 10, surfaceColor)
    val rectangleCommand = DrawCommand.Rectangle(0, 0, 30, 20, 0, surfaceColor)

    (() => painter.baseEntityColor).expects().returning(Right(entityColor)).once()
    (() => painter.baseSurfaceColor).expects().returning(Right(surfaceColor)).once()

    (() => mockState.allSurfaces).expects().returning(List(circleSurface, rectSurface))
    (() => mockState.allTeams).expects().returning(List.empty)
    (() => mockState.allEntities).expects().returning(List.empty)

    painter.drawCircle.expects(circleSurface, surfaceColor).returning(Some(circleCommand)).once()
    painter.drawRectangle
      .expects(rectSurface, surfaceColor)
      .returning(Some(rectangleCommand))
      .once()

    RendererManager.render(mockState).value shouldBe Vector(circleCommand, rectangleCommand)

  test("renderer manager should draw entities using their team color when teamId is present"):
    given painter: Painter = mock[Painter]

    val teamRedId    = TeamId("red").value
    val redColor     = EngineColorArranger.arrangeRed()
    val entityColor  = EngineColorArranger.arrangeWhite()
    val surfaceColor = EngineColorArranger.arrangeBlack()

    val team   = Team(teamRedId).value
    val entity = Entity.circle("e1", ZeroVector, 5.0).value.withTeamId(Some(teamRedId.value)).value
    val mockState = mock[State]
    val command   = DrawCommand.Circle(0, 0, 5, redColor)

    (() => painter.baseEntityColor).expects().returning(Right(entityColor)).once()
    (() => painter.baseSurfaceColor).expects().returning(Right(surfaceColor)).once()

    painter.teamIdColorRelation.expects(teamRedId).returning(Right(redColor)).once()

    (() => mockState.allSurfaces).expects().returning(List.empty)
    (() => mockState.allTeams).expects().returning(List(team))
    (() => mockState.allEntities).expects().returning(List(entity))

    painter.drawCircle.expects(entity, redColor).returning(Some(command)).once()

    RendererManager.render(mockState).value shouldBe Vector(command)

  test("renderer manager should draw entities using base color when teamId is None"):
    given painter: Painter = mock[Painter]

    val entityColor       = EngineColorArranger.arrangeWhite()
    val surfaceColor      = EngineColorArranger.arrangeRed()
    val entityWithoutTeam = Entity.rectangle("e2", ZeroVector, 10.0, 10.0).value
    val mockState         = mock[State]
    val command           = DrawCommand.Rectangle(0, 0, 10, 10, 0, entityColor)

    (() => painter.baseEntityColor).expects().returning(Right(entityColor)).once()
    (() => painter.baseSurfaceColor).expects().returning(Right(surfaceColor)).once()

    (() => mockState.allSurfaces).expects().returning(List.empty)
    (() => mockState.allTeams).expects().returning(List.empty)
    (() => mockState.allEntities).expects().returning(List(entityWithoutTeam))

    painter.drawRectangle
      .expects(entityWithoutTeam, entityColor)
      .returning(Some(command))
      .once()

    RendererManager.render(mockState).value shouldBe Vector(command)
