package monad_core.simulator.presentation.support

import scalafx.Includes.{jfxScene2sfx, jfxWindow2sfx}
import scalafx.scene.Node
import scalafx.stage.Window

/**
 * Singleton object containing utilities for all the presentation
 * components regarding some niche ScalaFx interactions
 */
private[presentation] object ScalaFxUtils:

  /**
   * tries to retrieve the Window of a given ScalaFx Node
   *
   * @param node the given node
   * @return `Some(Window)` if it's available, `None` otherwise
   */
  private def ownerWindowOf(node: Node): Option[Window] =
    Option(node.scene.value).flatMap(s => Option(s.window.value))

  /**
   * applies [[ownerWindowOf]] to a optional [[Node]]
   *
   * @see [[ownerWindowOf]]
   * @param node the optional node
   * @return `Some(Window)` if it's available, `None` otherwise
   */
  def ownerWindowOfOption(node: Option[Node]): Option[Window] =
    node.flatMap(ownerWindowOf)
