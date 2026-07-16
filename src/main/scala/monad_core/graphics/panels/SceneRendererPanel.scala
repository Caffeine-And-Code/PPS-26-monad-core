package monad_core.graphics.panels

import monad_core.engine.errors.EngineError
import monad_core.graphics.panels.support.{BaseLabelStyle, PanelStyles}
import monad_core.graphics.panels.traits.SceneRendererPanelBuilder
import scalafx.scene.control.Label
import scalafx.scene.layout.VBox

object SceneRendererPanel extends SceneRendererPanelBuilder {
  def build(): Either[EngineError, VBox] =
    Right(
      new VBox {
        children = Seq(
          new Label("Scene Renderer") {
            style = BaseLabelStyle.h1
          }
        )
        style = PanelStyles.base
      }
    )
}
