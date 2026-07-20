package monad_core.simulator.presentation.stages.traits

import dev.langchain4j.service.AiServices
import monad_core.engine.errors.EngineError
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.presentation.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord

import scala.concurrent.ExecutionContext

trait MainStageBuilder:
  def main()
          (
            using imageConfig: ImageConfigRecord,
            aiAgent: AiAgent,
            gameEnginePanelBuilder: GameEnginePanelBuilder,
            aiModelChatPanelBuilder: AiModelChatPanelBuilder,
            gameEngineModePanelBuilder: GameEngineModePanelBuilder,
            sceneRendererPanelBuilder: SceneRendererPanelBuilder,
            executionContext: ExecutionContext
          )
  : Option[EngineError]
