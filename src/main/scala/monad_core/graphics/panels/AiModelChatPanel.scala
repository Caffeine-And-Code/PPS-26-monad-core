package monad_core.graphics.panels

import monad_core.engine.errors.EngineError
import monad_core.graphics.panels.support.{BaseLabelStyle, PanelStyles}
import monad_core.graphics.panels.traits.AiModelChatPanelBuilder
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
        style = PanelStyles.base
      }
    )
}
