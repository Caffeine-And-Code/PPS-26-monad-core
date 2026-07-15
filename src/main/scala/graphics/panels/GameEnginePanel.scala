package graphics.panels

import engine.errors.EngineError
import graphics.CannotBuildPanel
import scalafx.scene.layout.VBox

object GameEnginePanel {
  private val TopPanelHeightRatio = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio = 0.02
  private val TopPanelMinHeight = 80.0

  def build(): Either[EngineError, VBox] =
    val gameEngineModePanelEither = GameEngineModePanel.build()
    val sceneRendererPanel = SceneRendererPanel.build()

    gameEngineModePanelEither match
      case Right(gameEngineModePanel) =>
        val container = new VBox {
          children = Seq(gameEngineModePanel, sceneRendererPanel)
        }

        container.spacing <== container.height * SpacingRatio

        gameEngineModePanel.prefHeight <== container.height * TopPanelHeightRatio
        gameEngineModePanel.minHeight = TopPanelMinHeight

        sceneRendererPanel.prefHeight <== container.height * BottomPanelHeightRatio

        Right(container)

      case Left(panelError) => Left(CannotBuildPanel(panelError, GameEnginePanel.toString))
}
