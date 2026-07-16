package monad_core.graphics.panels.traits

import monad_core.engine.errors.EngineError
import scalafx.scene.layout.VBox

trait AiModelChatPanelBuilder:
  def build() : Either[EngineError, VBox]
