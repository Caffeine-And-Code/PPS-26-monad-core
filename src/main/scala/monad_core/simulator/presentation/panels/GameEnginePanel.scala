package monad_core.simulator.presentation.panels

import monad_core.engine.errors.EngineError
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.presentation.panels.traits.{GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

object GameEnginePanel extends GameEnginePanelBuilder {
  private val TopPanelHeightRatio = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio = 0.02
  private val TopPanelMinHeight = 80.0

  def build()
           (
             using imageConfig: ImageConfigRecord,
             gameEngineModePanelBuilder: GameEngineModePanelBuilder,
             sceneRendererPanelBuilder: SceneRendererPanelBuilder
           )
  : Either[EngineError, VBox] =

    val gameEngineModePanelEither = gameEngineModePanelBuilder.build()
    val sceneRendererPanelEither = sceneRendererPanelBuilder.build()

    (gameEngineModePanelEither, sceneRendererPanelEither) match
      case (Right(gameEngineModePanel), Right(sceneRendererPanel)) =>
        val container = new VBox {
          children = Seq(gameEngineModePanel, sceneRendererPanel)
        }

        container.spacing <== container.height * SpacingRatio

        gameEngineModePanel.prefHeight <== container.height * TopPanelHeightRatio
        gameEngineModePanel.minHeight = TopPanelMinHeight

        sceneRendererPanel.prefHeight <== container.height * BottomPanelHeightRatio

        Right(container)

      case (Left(panelError), _) => Left(CannotBuildPanel(panelError, GameEnginePanel.toString))
      case (_, Left(panelError)) => Left(CannotBuildPanel(panelError, GameEnginePanel.toString))
}
