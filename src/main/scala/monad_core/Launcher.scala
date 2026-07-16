package monad_core

import monad_core.engine.errors.EngineError
import monad_core.graphics.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.graphics.panels.{AiModelChatPanel, GameEngineModePanel, GameEnginePanel, SceneRendererPanel}
import monad_core.graphics.resources.BaseImageConfig
import monad_core.graphics.stages.{MainStage, ScalaFxLauncher}

import scala.Console.{GREEN, RESET}

object Launcher:
  def main(args: Array[String]): Unit =
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

    val launcher = ScalaFxLauncher(mainStage)

    launcher.run() match
      case Left(error) =>
        Console.err.println(s"Startup failed: ${error.message}")
        sys.exit(1)

      case Right(_) =>
        Console.println(s"$RESET${GREEN}Build Completed$RESET")
