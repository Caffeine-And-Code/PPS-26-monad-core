package monad_core.simulator.presentation.panels.support

/**
 * Provides base styles for different types of Gui labels.
 */
object BaseLabelStyle:

  /**
   * Return the ScalaFx CSS to render a simple paragraph
   *
   * @return String that can be applied to a [[Node]] CSS
   */
  def p: String =
    "-fx-text-fill: #ffffff; -fx-font-size: 18px;"

  /**
   * Return the ScalaFx CSS to render a silent paragraph, a smaller text than [[p]],
   *      that is used to display little details in the Gui
   *
   * @return String that can be applied to a [[Node]] CSS
   */
  def silent: String =
    "-fx-text-fill: #dddddd; -fx-font-size: 16px;"

  /**
   * Return the ScalaFx CSS to render an H1 title
   *
   * @return String that can be applied to a [[Node]] CSS
   */
  def h1: String =
    p + "-fx-font-size: 23px; -fx-font-weight: bold;"
