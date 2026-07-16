package graphics.panels

import engine.errors.EngineError
import graphics.panels.support.{BaseLabelStyle, BasePanelStyle}
import graphics.panels.traits.SceneRendererPanelBuilder
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
