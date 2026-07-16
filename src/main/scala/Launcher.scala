import engine.errors.EngineError
import graphics.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import graphics.panels.{AiModelChatPanel, GameEngineModePanel, GameEnginePanel, SceneRendererPanel}
import graphics.resources.BaseImageConfig
import graphics.stages.MainStage

object Launcher {
  def main(args: Array[String]): Unit = {
    given imageConfig: BaseImageConfig = BaseImageConfig()

    given gameEnginePanelBuilder: GameEnginePanelBuilder = GameEnginePanel

    given aiModelChatPanelBuilder: AiModelChatPanelBuilder = AiModelChatPanel

    given gameEngineModePanelBuilder: GameEngineModePanelBuilder = GameEngineModePanel

    given sceneRendererPanelBuilder: SceneRendererPanelBuilder = SceneRendererPanel

    MainStage.main() match
      case Some(error: EngineError) => println(error.message)
      case None => println("Building Complete.\n")
  }
}