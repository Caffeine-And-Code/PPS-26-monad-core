package monad_core.engine.core

import monad_core.engine.core.traits.{RenderEngine, State}
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{EngineColor, EngineError, TeamId}
import monad_core.engine.simulator.Painter

object RendererManager extends RenderEngine:

  override def render(state: State)(using
      painter: Painter
  ): Either[EngineError, Unit] =
    for
      entityBaseColor <- painter.baseEntityColor
      surfacesColor   <- painter.baseSurfaceColor
      teamsMap <- state.allTeams.foldLeft[Either[EngineError, Map[TeamId, EngineColor]]](
        Right(Map.empty)
      ) { (acc, team) =>
        for
          map   <- acc
          color <- painter.teamIdColorRelation(team.id)
        yield map + (team.id -> color)
      }
    yield
      for (surface <- state.allSurfaces)
        surface.shape match
          case _: Circle    => painter.drawCircle(surface, surfacesColor)
          case _: Rectangle => painter.drawRectangle(surface, surfacesColor)

      def getTeamColorOrDefault(optionalTeamId: Option[TeamId]): EngineColor =
        optionalTeamId.flatMap(teamsMap.get).getOrElse(entityBaseColor)

      for (entity <- state.allEntities)
        entity.shape match
          case _: Circle    => painter.drawCircle(entity, getTeamColorOrDefault(entity.teamId))
          case _: Rectangle => painter.drawRectangle(entity, getTeamColorOrDefault(entity.teamId))
