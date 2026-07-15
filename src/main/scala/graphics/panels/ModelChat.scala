package graphics.panels

import graphics.panels.support.PanelStyle
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.VBox

object ModelChat {
  def build() : VBox =
    new VBox {
      children = Seq(
        new Label("Left Panel Content"),
        new Button("Action 1")
      )
      style = PanelStyle.get()
    }
}
