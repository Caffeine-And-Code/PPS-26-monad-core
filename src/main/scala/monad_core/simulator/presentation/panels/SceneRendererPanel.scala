package monad_core.simulator.presentation.panels

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.panels.support.{BaseLabelStyle, BasePanelStyle}
import monad_core.simulator.presentation.panels.traits.SceneRendererPanelBuilder
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
        style = BasePanelStyle.get()
      }
    )
}
