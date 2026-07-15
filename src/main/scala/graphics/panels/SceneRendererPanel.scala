package graphics.panels

import graphics.panels.support.{BaseLabelStyle, BasePanelStyle}
import scalafx.scene.control.Label
import scalafx.scene.layout.VBox

object SceneRendererPanel {
  def build(): VBox =
    new VBox {
      children = Seq(
        new Label("Scene Renderer") {
          style = BaseLabelStyle.h1
        }
      )
      style = BasePanelStyle.get()
    }
}
