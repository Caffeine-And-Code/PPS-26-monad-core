package monad_core.simulator.presentation.painters

import monad_core.engine.core.traits.{RenderEngine, State}
import monad_core.engine.model.{**, Locatable, TeamId}
import monad_core.engine.public_api.Painter
import scalafx.scene.paint.Color

object Drawer extends Painter:
  /**
   * defines a relation between TeamId and a Color, which will then be used to
   * color the entities of the team.
   *
   * @param teamId the id of the team
   * @return the associated color used to represent the entities of the team
   */
  override def teamIdColorRelation(teamId: TeamId): Color =
    Color.color(teamId.value.length ** 2, teamId.value.length ** 2, teamId.value.length ** 2)

  /**
   * @see drawCircle and drawRectangle
   * @return the default color if the entity is not in a team
   *         (this is also the color provided when the locatable is a surface)
   */
  override def baseColor: Color = Color.color(0, 0, 0)

  /**
   * specifies how circles are drawn by the engine
   *
   * @param locatable the element composed of shape and position to draw
   * @param color     the base color: if the entity is in a Team the color is
   *                  the team color, otherwise a default color is provided
   */
  override def drawCircle(locatable: Locatable, color: Color): Unit = ???

  /**
   * specifies how rectangles are drawn by the engine
   *
   * @param locatable the element composed of shape and position to draw
   * @param color     the base color: if the entity is in a Team the color is
   *                  the team color, otherwise a default color is provided
   */
  override def drawRectangle(locatable: Locatable, color: Color): Unit = ???

