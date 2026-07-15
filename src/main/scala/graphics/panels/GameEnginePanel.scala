package graphics.panels

import scalafx.scene.layout.VBox

object GameEnginePanel {
  private val TopPanelHeightRatio = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio = 0.02
  private val TopPanelMinHeight = 80.0

  def build(): VBox =
    val gameEngineModePanel = GameEngineModePanel.build()
    val sceneRendererPanel = SceneRendererPanel.build()

    val container = new VBox {
      children = Seq(gameEngineModePanel, sceneRendererPanel)
    }

    container.spacing <== container.height * SpacingRatio

    gameEngineModePanel.prefHeight <== container.height * TopPanelHeightRatio
    gameEngineModePanel.minHeight = TopPanelMinHeight

    sceneRendererPanel.prefHeight <== container.height * BottomPanelHeightRatio

    container
}
