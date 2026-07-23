package monad_core

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.panels.{AiModelChatPanel, GameEngineModePanel, GameEnginePanel, SceneRendererPanel}
import monad_core.simulator.presentation.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.BaseImageConfig
import monad_core.simulator.presentation.stages.MainStage
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.application.engine.{GameEngineRuntime, Word}
import monad_core.simulator.infrastructure.ai.{Langchain4jAgentFactory, Langchain4jOllamaConfig}
import monad_core.simulator.infrastructure.engine.{DummyGameEngineRuntime, EngineWord}

import scala.concurrent.ExecutionContext.Implicits.global

object Launcher {
  def main(args: Array[String]): Unit = {

    given word: Word = EngineWord()

    given gameEngineRuntime :GameEngineRuntime = DummyGameEngineRuntime()

    given aiAgent: AiAgent = Langchain4jAgentFactory
      .buildOllama(
        Langchain4jOllamaConfig(
          url = "http://localhost:11434",
          modelName = "gemma4:e2b"
        )
      )

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
