package monad_core.engine.core

import monad_core.engine.core.traits.{RenderEngine, State}
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.TeamId
import monad_core.engine.simulator.Painter
import scalafx.scene.paint.Color

object RendererManager extends RenderEngine:

  override def render(state: State)(using painter: Painter): Unit =
    for (surface <- state.allSurfaces)
      surface.shape match
        case _: Circle    => painter.drawCircle(surface, painter.baseColor)
        case _: Rectangle => painter.drawRectangle(surface, painter.baseColor)

    val teamsMap: Map[TeamId, Color] = Map.from(
      state.allTeams.map(team => (team.id, painter.teamIdColorRelation(team.id)))
    )

    def getTeamColorOrDefault(optionalTeamId: Option[TeamId]): Color =
      if optionalTeamId.isEmpty then painter.baseColor
      else teamsMap.getOrElse(optionalTeamId.get, painter.baseColor)

    for (entity <- state.allEntities)
      entity.shape match
        case _: Circle    => painter.drawCircle(entity, getTeamColorOrDefault(entity.teamId))
        case _: Rectangle => painter.drawRectangle(entity, getTeamColorOrDefault(entity.teamId))
