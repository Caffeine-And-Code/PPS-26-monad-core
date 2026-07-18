package monad_core.simulator.presentation.panels.traits

import monad_core.engine.errors.EngineError
import monad_core.simulator.application.AgentService
import scalafx.scene.layout.VBox

import scala.concurrent.ExecutionContext

trait AiModelChatPanelBuilder:
  def build()(using AgentService, ExecutionContext): Either[EngineError, VBox]
