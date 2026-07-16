package graphics.stages.traits

import engine.errors.EngineError
import graphics.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import graphics.resources.ImageConfigRecord

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
