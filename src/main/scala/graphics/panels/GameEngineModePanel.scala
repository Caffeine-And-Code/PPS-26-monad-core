package graphics.panels

import graphics.panels.support.{BaseLabelStyle, BasePanelStyle}
import scalafx.scene.control.Label
import scalafx.scene.layout.VBox

object GameEngineModePanel {
  def build():VBox =
    new VBox {
      children = Seq(
        new Label("Top Panel To Handle The GameEngine Status") {
          style = BaseLabelStyle.h1
        }
      )
      style =
        BasePanelStyle.get()
    }
}
