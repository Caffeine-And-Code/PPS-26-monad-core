package graphics.panels

import graphics.panels.support.{BaseLabelStyle, BasePanelStyle}
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.VBox

object AiModelChatPanel {
  def build() : VBox =
    new VBox {
      children = Seq(
        new Label("Left Panel Content") {
          style = BaseLabelStyle.h1
        },
        new Button("Action 1")
      )
      style = BasePanelStyle.get()
    }
}
