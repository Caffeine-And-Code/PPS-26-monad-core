package monad_core.simulator.presentation.stages.traits

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord

trait MainStageBuilder:
  def main()
          (
            using imageConfig: ImageConfigRecord,
            gameEnginePanelBuilder: GameEnginePanelBuilder,
            aiModelChatPanelBuilder: AiModelChatPanelBuilder,
            gameEngineModePanelBuilder: GameEngineModePanelBuilder,
            sceneRendererPanelBuilder: SceneRendererPanelBuilder
          )
  : Option[EngineError]
