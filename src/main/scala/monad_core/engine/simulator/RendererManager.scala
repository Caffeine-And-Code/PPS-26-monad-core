package monad_core.engine.simulator

import monad_core.engine.core.traits.{RenderEngine, State}
import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}

/**
 * Pure renderer that converts an engine state into backend-independent drawing commands.
 *
 * Surfaces are emitted before entities. Entities use their team color when available and the painter's base entity
 * color otherwise.
 */
object RendererManager extends RenderEngine:

  private def determineCommand(
      shape: Shape2D,
      color: EngineColor,
      locatable: Locatable
  )(using
      painter: Painter
  ): Option[DrawCommand] =
    shape match
      case _: Circle    => painter.drawCircle(locatable, color)
      case _: Rectangle => painter.drawRectangle(locatable, color)

  /**
   * Produces the ordered drawing plan for a state.
   *
   * Color construction is evaluated before commands are produced; the first painter error stops rendering.
   *
   * @param state
   *   state whose surfaces, entities and teams are rendered
   * @param painter
   *   strategy used to select colors and translate shapes into commands
   * @return
   *   the ordered drawing commands, or the first color-construction error
   */
  override def render(state: State)(using
      painter: Painter
  ): Either[EngineError, Vector[DrawCommand]] =
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
      val surfaceCommands = state.allSurfaces.flatMap(surface =>
        determineCommand(surface.shape, surfacesColor, surface)
      )

      val entityCommands = state.allEntities.flatMap { entity =>
        val teamColor = entity.teamId.flatMap(teamsMap.get).getOrElse(entityBaseColor)

        determineCommand(entity.shape, teamColor, entity)
      }

      (surfaceCommands ++ entityCommands).toVector
