package monad_core.engine.simulator

import monad_core.engine.model.{EngineColor, EngineError, Locatable, TeamId}

trait Painter:

  /**
   * @see drawCircle and drawRectangle
   * @return the default color if the entity is not in a team
   */
  def baseEntityColor: Either[EngineError, EngineColor]

  /**
   * @see drawCircle and drawRectangle
   * @return the color utilized for the surfaces
   */
  def baseSurfaceColor: Either[EngineError, EngineColor]

  /**
   * specifies how circles are drawn by the engine
   *
   * @param locatable the element composed of shape and position to draw
   * @param color the base color: if the entity is in a Team the color is
   *              the team color, otherwise a default color is provided
   */
  def drawCircle(locatable: Locatable, color: EngineColor): Unit

  /**
   * specifies how rectangles are drawn by the engine
   *
   * @param locatable the element composed of shape and position to draw
   * @param color the base color: if the entity is in a Team the color is
   *              the team color, otherwise a default color is provided
   */
  def drawRectangle(locatable: Locatable, color: EngineColor): Unit

  /**
   * defines a relation between TeamId and a Color, which will then be used to
   * color the entities of the team.
   *
   * @param teamId the id of the team
   * @return the associated color used to represent the entities of the team
   */
  def teamIdColorRelation(teamId: TeamId): Either[EngineError, EngineColor]
