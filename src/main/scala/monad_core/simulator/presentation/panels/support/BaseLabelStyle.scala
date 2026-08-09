package monad_core.simulator.presentation.panels.support

object BaseLabelStyle {
  def p: String =
    "-fx-text-fill: #ffffff; -fx-font-size: 18px;"

  def silent: String =
    "-fx-text-fill: #dddddd; -fx-font-size: 16px;"

  def h1: String =
    p + "-fx-font-size: 23px; -fx-font-weight: bold;"
}
