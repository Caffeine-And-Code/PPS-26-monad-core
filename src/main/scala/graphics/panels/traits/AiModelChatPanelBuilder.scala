package graphics.panels.traits

import engine.errors.EngineError
import scalafx.scene.layout.VBox

trait AiModelChatPanelBuilder:
  def build() : Either[EngineError, VBox]
