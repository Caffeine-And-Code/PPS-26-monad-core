package monad_core.engine.simulator

import monad_core.engine.model.{EngineColor, EngineError, Locatable, TeamId}

/**
 * Rendering policy used by the engine to convert model elements into backend-neutral
 * [[DrawCommand]] values.
 *
 * Implementations define the color palette and how supported shapes are represented as
 * drawing commands. They do not draw directly on a UI surface.
 */
trait Painter:

  /**
   * Provides the color used for entities without a team-specific color.
   *
   * @return the default entity color, or an engine error if it cannot be constructed
   */
  def baseEntityColor: Either[EngineError, EngineColor]

  /**
   * Provides the default color used for surfaces.
   *
   * @return the surface color, or an engine error if it cannot be constructed
   */
  def baseSurfaceColor: Either[EngineError, EngineColor]

  /**
   * Converts a circular locatable into a drawing command.
   *
   * @param locatable positioned model element to convert
   * @param color color assigned to the resulting command
   * @return `Some(DrawCommand.Circle)` when the locatable is circular, or `None` for another shape
   */
  def drawCircle(locatable: Locatable, color: EngineColor): Option[DrawCommand]

  /**
   * Converts a rectangular locatable into a drawing command.
   *
   * @param locatable positioned model element to convert
   * @param color color assigned to the resulting command
   * @return `Some(DrawCommand.Rectangle)` when the locatable is rectangular, or `None` for another shape
   */
  def drawRectangle(locatable: Locatable, color: EngineColor): Option[DrawCommand]

  /**
   * Resolves the color associated with a team.
   *
   * @param teamId identifier of the team
   * @return the team color, or an engine error if it cannot be constructed
   */
  def teamIdColorRelation(teamId: TeamId): Either[EngineError, EngineColor]
