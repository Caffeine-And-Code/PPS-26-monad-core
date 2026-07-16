package monad_core.graphics.stages.support

import scalafx.geometry.Insets

case class Padding(top: Int, bottom: Int, left: Int, right: Int)

object Padding:
  def symmetrical(horizontal: Int, vertical: Int): Padding =
    Padding(vertical, vertical, horizontal, horizontal)

extension (padding: Padding)

  def horizontalSpacing: Int = padding.left + padding.right

  def verticalSpacing: Int = padding.top + padding.bottom

  def toInsets: Insets =
    Insets(padding.top, padding.right, padding.bottom, padding.left)