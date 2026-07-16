package monad_core.graphics.panels

import monad_core.engine.errors.EngineError
import monad_core.graphics.CannotBuildPanel
import monad_core.graphics.components.{IconButton, IconButtonBaseProps}
import monad_core.graphics.panels.support.PanelStyles
import monad_core.graphics.panels.traits.GameEngineModePanelBuilder
import monad_core.graphics.resources.Image.{PauseIcon, PlayIcon, StopIcon}
import monad_core.graphics.resources.ImageConfigRecord
import scalafx.geometry.Pos
import scalafx.scene.layout.{HBox, Priority, VBox}

object GameEngineModePanel extends GameEngineModePanelBuilder {
  def build(imageConfig: ImageConfigRecord): Either[EngineError, VBox] =
    for
      
      playPauseBtn <- IconButton.buildToggle(
          defaultImage = PlayIcon(),
          activeImage = PauseIcon(),
          props= IconButtonBaseProps(imageConfig)
        )
        .left.map(error => CannotBuildPanel(error, GameEngineModePanel.toString))
      
      stopBtn <- IconButton.build(StopIcon(), IconButtonBaseProps(imageConfig, isDisabled = true))
        .left.map(error => CannotBuildPanel(error, GameEngineModePanel.toString))
    yield
      new VBox {
        val buttonsRow: HBox = new HBox {
          spacing = 8
          alignment = Pos.CenterRight
          children = Seq(
            playPauseBtn,
            stopBtn
          )
        }

        VBox.setVgrow(buttonsRow, Priority.Always)

        children = Seq(buttonsRow)
        alignment = Pos.BottomRight
        style = PanelStyles.base
      }
}
