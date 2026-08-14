package monad_core

import monad_core.engine.errors.EngineError
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.infrastructure.ai.{Langchain4jAgentFactory, Langchain4jOllamaConfig}
import monad_core.simulator.infrastructure.engine.{MonadCodeGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.presentation.routes.RouteType.{All, Route}
import monad_core.simulator.presentation.routes.{Router, ArgumentRoutingRoute, RouteResponse}
import monad_core.simulator.presentation.panels.{
  AiModelChatPanel,
  GameEngineModePanel,
  GameEnginePanel,
  SceneRendererPanel
}
import monad_core.simulator.presentation.resources.BaseImageConfig
import monad_core.simulator.presentation.stages.{MainStage, ScalaFxLauncher}

import scala.Console.{GREEN, RESET}

object Launcher:

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

  def outcomeFor(result: Either[EngineError, Unit]): RouteResponse =
    result match
      case Left(error) =>
        RouteResponse(success = false, message = s"Startup failed: ${error.message}")
      case Right(_) => RouteResponse(success = true, message = s"${GREEN}Build Completed$RESET")

  private def evaluateModel(): RouteResponse =
    RouteResponse(
      success = true,
      message = "Model evaluated"
    )

  def main(args: Array[String]): Unit =

    given world: World = MonadCoreWorld()

    given gameEngine: GameEngineRuntime = MonadCodeGameEngineRuntime()

    given aiAgent: AiAgent = Langchain4jAgentFactory
      .buildOllama(
        Langchain4jOllamaConfig(
          url = sys.env.getOrElse("MONAD_CORE_OLLAMA_URL", "http://localhost:11434"),
          modelName = sys.env.getOrElse("MONAD_CORE_MODEL_NAME", "gemma4:e4b")
        )
      )

    lazy val evaluateModelRoute = evaluateModel()
    lazy val guiRoute           = outcomeFor(buildLauncher().run())

    val result = Router()
      .on(Route("evaluate-model"), () => evaluateModelRoute)
      .on(All(), () => guiRoute)
      .evaluate(args)

    result match
      case Left(error) =>
        Console.err.println(error.message)
        sys.exit(1)
      case Right(response) =>
        if response.success then Console.println(s"$RESET${response.message}")
        else
          Console.err.println(response.message)
          sys.exit(1)
