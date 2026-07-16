package monad_core.graphics.stages.traits

import monad_core.engine.errors.EngineError
import monad_core.graphics.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.graphics.resources.ImageConfigRecord

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
