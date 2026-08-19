package monad_core

import monad_core.engine.simulator.Painter
import monad_core.simulator.application.ai.{AgentEvaluationDataset, AgentEvaluator, AiAgent}
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.application.engine.{GameEngineRuntime, ShapeArchitect}
import monad_core.simulator.application.logging.Logger
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.agent_evaluator.Langchain4jAgentEvaluator
import monad_core.simulator.infrastructure.ai.agent_evaluator.dataset.HardcodedAgentEvaluationDataset
import monad_core.simulator.infrastructure.ai.{Langchain4jAgentFactory, Langchain4jOllamaConfig}
import monad_core.simulator.infrastructure.engine.painters.PaintArchitect
import monad_core.simulator.infrastructure.engine.{MonadCoreGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.infrastructure.logging.ConsoleLogger
import monad_core.simulator.presentation.agent_evaluation.{
  AgentEvaluationArguments,
  AgentEvaluationRuntime,
  AgentEvaluatorConsolePrinter,
  AgentEvaluatorPrinter
}
import monad_core.simulator.presentation.components.{Error, NotificationManager}
import monad_core.simulator.presentation.panels.{
  AiModelChatPanel,
  GameEngineModePanel,
  GameEnginePanel,
  SceneRendererPanel
}
import monad_core.simulator.presentation.resources.BaseImageConfig
import monad_core.simulator.presentation.routes.RouteType.{All, Route}
import monad_core.simulator.presentation.routes.{RouteResponse, Router}
import monad_core.simulator.presentation.stages.{MainStage, ScalaFxLauncher}

import scala.Console.{GREEN, RESET}

object Launcher:

  private def guiApplication(): Either[BaseError, Unit] =
    given World = MonadCoreWorld()

    given GameEngineRuntime = MonadCoreGameEngineRuntime { error =>
      NotificationManager.show(error.message, Error)
    }

    given painter: Painter = PaintArchitect

    given architect: ShapeArchitect = PaintArchitect

    given AiAgent = Langchain4jAgentFactory
      .buildOllama(
        Langchain4jOllamaConfig(
          url = sys.env.getOrElse("MONAD_CORE_OLLAMA_URL", "http://localhost:11434"),
          modelName = sys.env.getOrElse("MONAD_CORE_MODEL_NAME", "gemma4:e4b")
        )
      )

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

    ScalaFxLauncher(mainStage).run()

  def outcomeFor(result: Either[BaseError, Unit]): RouteResponse =
    result match
      case Left(error) =>
        RouteResponse(success = false, message = s"Startup failed: ${error.message}")
      case Right(_) => RouteResponse(success = true, message = s"${GREEN}Build Completed$RESET")

  private def evaluateModel(args: Array[String]): RouteResponse = {
    Console.println("Started model evaluation")

    val arguments = AgentEvaluationArguments.parse(args)

    given AgentEvaluatorPrinter = AgentEvaluatorConsolePrinter
    given Logger                = ConsoleLogger
    given AgentEvaluator = Langchain4jAgentEvaluator.buildOllama(
      agentConfig = Langchain4jOllamaConfig(
        url = arguments.testModelUrl,
        modelName = arguments.testModel
      ),
      judgeConfig = Langchain4jOllamaConfig(
        url = arguments.judgeModelUrl,
        modelName = arguments.judgeModel
      )
    )
    given AgentEvaluationDataset = HardcodedAgentEvaluationDataset

    AgentEvaluationRuntime.handle()

    RouteResponse(
      success = true,
      message = "Finished model evaluation"
    )
  }

  def main(args: Array[String]): Unit =
    lazy val evaluateModelRoute = evaluateModel(args)
    lazy val guiRoute           = outcomeFor(guiApplication())

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
