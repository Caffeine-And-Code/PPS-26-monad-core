package monad_core.graphics.panels

import monad_core.engine.errors.EngineError
import monad_core.graphics.CannotBuildPanel
import monad_core.graphics.panels.traits.{GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.graphics.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

final class GameEnginePanel(
                             modePanel: GameEngineModePanelBuilder,
                             rendererPanel: SceneRendererPanelBuilder,
                             imageConfig: ImageConfigRecord
                           ) extends GameEnginePanelBuilder {

  private val TopPanelHeightRatio = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio = 0.02
  private val TopPanelMinHeight = 80.0

  def build(): Either[EngineError, VBox] =
    for
      gameEngineModePanel <- modePanel.build(imageConfig)
        .left.map(error => CannotBuildPanel(error, this.toString))
      sceneRendererPanel <- rendererPanel.build()
        .left.map(error => CannotBuildPanel(error, this.toString))
    yield
      val container = new VBox {
        children = Seq(gameEngineModePanel, sceneRendererPanel)
      }

      container.spacing <== container.height * SpacingRatio

      gameEngineModePanel.prefHeight <== container.height * TopPanelHeightRatio
      gameEngineModePanel.minHeight = TopPanelMinHeight

      sceneRendererPanel.prefHeight <== container.height * BottomPanelHeightRatio

      container
}