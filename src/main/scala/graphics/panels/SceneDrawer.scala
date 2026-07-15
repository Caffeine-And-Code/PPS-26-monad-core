package graphics.panels

import graphics.panels.support.PanelStyle
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.VBox

object SceneDrawer {
  def build(): VBox =
    new VBox {
      children = Seq(
        new Label("Right Panel Content"),
        new Button("Action 2")
      )
      style = PanelStyle.get()
    }
  
}
