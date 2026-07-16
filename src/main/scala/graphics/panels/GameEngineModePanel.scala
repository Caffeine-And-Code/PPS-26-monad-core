package graphics.panels

import engine.errors.EngineError
import graphics.CannotBuildPanel
import graphics.components.IconButton
import graphics.panels.support.BasePanelStyle
import graphics.panels.traits.GameEngineModePanelBuilder
import graphics.resources.Image.{PauseIcon, PlayIcon, StopIcon}
import graphics.resources.ImageConfigRecord
import scalafx.geometry.Pos
import scalafx.scene.layout.{HBox, Priority, VBox}

object GameEngineModePanel extends GameEngineModePanelBuilder {
  def build()
           (using imageConfig: ImageConfigRecord)
  : Either[EngineError, VBox] =
    val playPauseBtnEither = IconButton.buildToggle(PlayIcon(), PauseIcon())
    val stopBtnEither = IconButton.build(StopIcon(), isDisabled = true)

    (playPauseBtnEither, stopBtnEither) match
      case (Right(builtPlayPauseBtn), Right(builtStopBtn)) =>
        Right(
          new VBox {
            val buttonsRow: HBox = new HBox {
              spacing = 8
              alignment = Pos.CenterRight
              children = Seq(
                builtPlayPauseBtn,
                builtStopBtn
              )
            }

            VBox.setVgrow(buttonsRow, Priority.Always)

            children = Seq(buttonsRow)
            alignment = Pos.BottomRight
            style = BasePanelStyle.get()
          }
        )

      case (Left(playPauseError), _) => Left(CannotBuildPanel(playPauseError, GameEngineModePanel.toString))
      case (_, Left(stopError)) => Left(CannotBuildPanel(stopError, GameEngineModePanel.toString))
}
