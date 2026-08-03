package monad_core

import monad_core.engine.errors.EngineError
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.infrastructure.ai.{Langchain4jAgentFactory, Langchain4jOllamaConfig, Langchain4jTools}
import monad_core.simulator.infrastructure.engine.{MonadCodeGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.{Langchain4jAgentFactory, Langchain4jOllamaConfig}
import monad_core.simulator.infrastructure.engine.{MonadCodeGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.presentation.panels.{AiModelChatPanel, GameEngineModePanel, GameEnginePanel, SceneRendererPanel}
import monad_core.simulator.presentation.resources.BaseImageConfig
import monad_core.simulator.presentation.stages.{MainStage, ScalaFxLauncher}

import scala.Console.{GREEN, RESET}

object Launcher :
  private def buildLauncher()(using World, GameEngineRuntime): ScalaFxLauncher =
    val imageConfig = BaseImageConfig()

    val gamePanel = GameEnginePanel(
      modePanel = GameEngineModePanel,
      rendererPanel = SceneRendererPanel,
      imageConfig = imageConfig
    )

    val mainStage = MainStage(
      gamePanel = gamePanel,
      chatPanel = AiModelChatPanel
    )

    ScalaFxLauncher(mainStage)

  def outcomeFor(result: Either[BaseError, Unit]): (Boolean, String) =
    result match
      case Left(error) => (false, s"Startup failed: ${error.message}")
      case Right(_)     => (true, s"${GREEN}Build Completed$RESET")

  def main(args: Array[String]): Unit =

    given world: World = MonadCoreWorld()

    given gameEngine: GameEngineRuntime = MonadCodeGameEngineRuntime()

    given aiAgent: AiAgent = Langchain4jAgentFactory
      .buildOllama(
        Langchain4jOllamaConfig(
          url = sys.env.getOrElse("MONAD_CORE_OLLAMA_URL", "http://localhost:11434"),
          modelName = sys.env.getOrElse("MONAD_CORE_MODEL_NAME", "qwen3.5:4b")
        )
      )

    val (success, message) = outcomeFor(buildLauncher().run())

    if success then
      Console.println(s"$RESET$message")
    else
      Console.err.println(message)
      sys.exit(1)