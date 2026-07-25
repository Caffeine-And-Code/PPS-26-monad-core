package monad_core.simulator.presentation.panels

import monad_core.engine.errors.EngineError
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.presentation.components.{IconButton, IconButtonBaseProps}
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.resources.Image.{PauseIcon, PlayIcon, StopIcon}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Pos
import scalafx.scene.layout.{HBox, Priority, VBox}

object GameEngineModePanel extends GameEngineModePanelBuilder {
  def build(
             imageConfig: ImageConfigRecord,
             onModeChange: Boolean => Unit,
             onStopClick: () => Unit
           ): Either[EngineError, VBox] = {
    val isRunning = BooleanProperty(false)

    for
      playPauseBtn <- IconButton.buildToggle(
          defaultImage = PlayIcon(),
          activeImage = PauseIcon(),
          props = IconButtonBaseProps(
            imageConfig,
            onClick =
              isActive =>
                isRunning.value = isActive
                onModeChange(isActive)
          ),
          activeProperty = isRunning
        )
        .left.map(error => CannotBuildPanel(error, GameEngineModePanel.toString))

      stopBtn <- IconButton.build(
          StopIcon(),
          IconButtonBaseProps(
            imageConfig,
            isDisabled = !isRunning,
            onClick = isActive =>
              isRunning.value = false
              onStopClick()
          )
        )
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
}
