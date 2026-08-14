package monad_core.simulator.presentation.support

import scalafx.Includes.{jfxScene2sfx, jfxWindow2sfx}
import scalafx.scene.Node
import scalafx.stage.Window

private[presentation] object ScalaFxUtils {

  def ownerWindowOf(node: Node): Option[Window] =
    Option(node.scene.value).flatMap(s => Option(s.window.value))

  def ownerWindowOfOption(node: Option[Node]): Option[Window] =
    node.flatMap(ownerWindowOf)

}
