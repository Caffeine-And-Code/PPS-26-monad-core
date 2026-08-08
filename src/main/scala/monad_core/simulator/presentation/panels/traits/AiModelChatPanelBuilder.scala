package monad_core.simulator.presentation.panels.traits

import monad_core.engine.errors.EngineError
import monad_core.simulator.application.ai.AiAgent
import scalafx.scene.layout.VBox

import scala.concurrent.ExecutionContext

trait AiModelChatPanelBuilder:
  def build(aiAgent: AiAgent)(using ExecutionContext): Either[EngineError, VBox]
