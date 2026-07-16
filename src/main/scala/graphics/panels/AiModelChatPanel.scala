package graphics.panels

import engine.errors.EngineError
import graphics.panels.support.{BaseLabelStyle, BasePanelStyle}
import graphics.panels.traits.AiModelChatPanelBuilder
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.VBox

object AiModelChatPanel extends AiModelChatPanelBuilder{
  def build() : Either[EngineError, VBox] =
    Right(
      new VBox {
        children = Seq(
          new Label("Left Panel Content") {
            style = BaseLabelStyle.h1
          },
          new Button("Action 1")
        )
        style = BasePanelStyle.get()
      }
    )
}
