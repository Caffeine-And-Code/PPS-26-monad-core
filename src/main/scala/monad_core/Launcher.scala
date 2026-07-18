package monad_core

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.panels.{AiModelChatPanel, GameEngineModePanel, GameEnginePanel, SceneRendererPanel}
import monad_core.simulator.presentation.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.BaseImageConfig
import monad_core.simulator.presentation.stages.MainStage
import monad_core.simulator.application.AgentService.given

import scala.concurrent.ExecutionContext.Implicits.global

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
