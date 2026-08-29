package monad_core

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.simulator.Painter
import monad_core.performance.simulator.PerformanceCli
import monad_core.simulator.application.ai.{AgentEvaluationDataset, AgentEvaluator, AiAgent}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.application.logging.Logger
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.agent_evaluator.Langchain4jAgentEvaluator
import monad_core.simulator.infrastructure.ai.agent_evaluator.dataset.HardcodedAgentEvaluationDataset
import monad_core.simulator.infrastructure.ai.{Langchain4jAgentFactory, Langchain4jOllamaConfig}
import monad_core.simulator.infrastructure.engine.painters.PaintArchitect
import monad_core.simulator.infrastructure.engine.{MonadCoreGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.infrastructure.logging.{
  ConsoleLogger,
  EventLogEntry,
  EventLogLevel,
  mapEventsToLogEntries
}
import monad_core.simulator.presentation.agent_evaluation.{
  AgentEvaluationArguments,
  AgentEvaluationRuntime,
  AgentEvaluatorConsolePrinter,
  AgentEvaluatorPrinter
}
import monad_core.simulator.presentation.components.{Error, NotificationManager}
import monad_core.simulator.presentation.panels.{
  AiModelChatPanel,
  GameEnginePanel,
  SceneRendererPanel
}
import monad_core.simulator.presentation.performance.PerformanceMode
import monad_core.simulator.presentation.resources.BaseImageConfig
import monad_core.simulator.presentation.routes.RouteType.{All, Route}
import monad_core.simulator.presentation.routes.{RouteResponse, Router}
import monad_core.simulator.presentation.stages.{MainStage, ScalaFxLauncher}

import scala.Console.{GREEN, RESET}

/**
 * Application entry point and command-line router.
 *
 * It starts the model-evaluation suite when the `evaluate-model` argument is present;
 * otherwise, it launches the GUI application. The `--performance` option decorates the GUI
 * with its optional performance control.
 */
object Launcher:

  /**
   * Assembles the dependencies required by the GUI and starts the ScalaFX application.
   *
   * The runtime, world, painter, AI agent, and panels are wired before control is delegated
   * to [[monad_core.simulator.presentation.stages.ScalaFxLauncher ScalaFxLauncher]].
   *
   * @param args application command-line arguments controlling optional GUI features
   * @return `Left(BaseError)` if the UI cannot be initialized, or `Right(Unit)` once it is started
   */
  private def guiApplication(args: Array[String]): Either[BaseError, Unit] =
    given Logger = ConsoleLogger

    val logger = summon[Logger]
    val logEvents: Vector[EngineEvent] => Unit = events =>
      mapEventsToLogEntries(events).foreach:
        case EventLogEntry(EventLogLevel.Info, message)  => logger.info(message)
        case EventLogEntry(EventLogLevel.Trace, message) => logger.trace(message)

    val runtime = MonadCoreGameEngineRuntime(
      onError = error => NotificationManager.show(error.message, Error),
      onEvents = logEvents
    )
    given GameEngineRuntime = runtime

    given World = MonadCoreWorld(
      onEvents = logEvents
    )

    given painter: Painter = PaintArchitect

    given AiAgent = Langchain4jAgentFactory
      .buildOllama(
        Langchain4jOllamaConfig(
          url = sys.env.getOrElse("MONAD_CORE_OLLAMA_URL", "http://localhost:11434"),
          modelName = sys.env.getOrElse("MONAD_CORE_MODEL_NAME", "gemma4:e4b")
        )
      )

    val imageConfig = BaseImageConfig()

    val modePanel = PerformanceMode.panelFor(
      args,
      () => runtime.physicsManagerSnapshot
    )

    val gamePanel = GameEnginePanel(
      modePanel = modePanel,
      rendererPanel = SceneRendererPanel,
      imageConfig = imageConfig
    )

    val mainStage = MainStage(
      gamePanel = gamePanel,
      chatPanel = AiModelChatPanel
    )

    ScalaFxLauncher(mainStage).run()

  /**
   * Converts the outcome of an application startup into a response suitable for command-line routing.
   *
   * @param result startup result to convert
   * @return a successful response when `result` is `Right`, or a failure response containing the error message
   */
  def outcomeFor(result: Either[BaseError, Unit]): RouteResponse =
    result match
      case Left(error) =>
        RouteResponse(success = false, message = s"Startup failed: ${error.message}")
      case Right(_) => RouteResponse(success = true, message = s"${GREEN}Build Completed$RESET")

  private def evaluateModel(args: Array[String]): RouteResponse = {
    Console.println("Started model evaluation")

    val arguments = AgentEvaluationArguments.parse(args)

    given AgentEvaluatorPrinter = AgentEvaluatorConsolePrinter

    given Logger = ConsoleLogger

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

  /**
   * Routes command-line arguments to model evaluation or to the default GUI application.
   *
   * The process exits with status `1` when routing or application startup fails.
   *
   * @see [[monad_core.simulator.presentation.routes.RouteType.Route Route]]
   * @see [[monad_core.simulator.presentation.routes.Router Router]]
   * @param args command-line arguments
   */
  def main(args: Array[String]): Unit =
    lazy val evaluateModelRoute = evaluateModel(args)
    lazy val performanceLoadRoute =
      PerformanceMode.runCommand(args, PerformanceCli.LoadRoute)
    lazy val performanceStressRoute =
      PerformanceMode.runCommand(args, PerformanceCli.StressRoute)
    lazy val performanceSpikeRoute =
      PerformanceMode.runCommand(args, PerformanceCli.SpikeRoute)
    lazy val performanceScalabilityRoute =
      PerformanceMode.runCommand(args, PerformanceCli.ScalabilityRoute)
    lazy val guiRoute = outcomeFor(guiApplication(args))

    val result = Router()
      .on(Route("evaluate-model"), () => evaluateModelRoute)
      .on(Route(PerformanceCli.LoadRoute), () => performanceLoadRoute)
      .on(Route(PerformanceCli.StressRoute), () => performanceStressRoute)
      .on(Route(PerformanceCli.SpikeRoute), () => performanceSpikeRoute)
      .on(Route(PerformanceCli.ScalabilityRoute), () => performanceScalabilityRoute)
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
