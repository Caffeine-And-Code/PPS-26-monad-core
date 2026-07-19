package monad_core

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.panels.{AiModelChatPanel, GameEngineModePanel, GameEnginePanel, SceneRendererPanel}
import monad_core.simulator.presentation.resources.BaseImageConfig
import monad_core.simulator.presentation.stages.{MainStage, ScalaFxLauncher}

import scala.Console.{GREEN, RESET}

object Launcher:
  def buildLauncher(): ScalaFxLauncher =
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

  def outcomeFor(result: Either[EngineError, Unit]): (Boolean, String) =
    result match
      case Left(error) => (false, s"Startup failed: ${error.message}")
      case Right(_)     => (true, s"${GREEN}Build Completed$RESET")

  def main(args: Array[String]): Unit =
    val (success, message) = outcomeFor(buildLauncher().run())

    if success then
      Console.println(s"$RESET$message")
    else
      Console.err.println(message)
      sys.exit(1)
