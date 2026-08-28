package monad_core.simulator.presentation.performance

import javafx.scene.layout.HBox as JfxHBox
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.{IconButton, IconButtonBaseProps, MenuButton}
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.resources.Image.PerformanceIcon
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

import scala.jdk.CollectionConverters.*

/**
 * Adds the optional performance control to an existing engine-mode panel builder.
 *
 * @param delegate
 *   base panel builder whose controls and behaviour are preserved
 * @param onPerformanceExperiment
 *   callback invoked when the performance control is selected
 */
final case class PerformanceGameEngineModePanel(
    delegate: GameEngineModePanelBuilder,
    onPerformanceExperiment: () => Unit
) extends GameEngineModePanelBuilder:

  private val PerformanceButtonIndex = 2

  /**
   * Builds the base panel and inserts its optional performance control.
   *
   * @param imageConfig
   *   image-loading configuration used by the control
   * @param onModeChange
   *   callback invoked when the engine mode changes
   * @param onStopClick
   *   callback invoked when the engine is reset
   * @param isEngineRunning
   *   observable engine state used to disable the performance control
   * @param world
   *   world edited by the base panel
   * @param gameEngineRuntime
   *   runtime controlled by the base panel
   * @return
   *   decorated panel, or the first base-panel or button-building error
   */
  override def build(
      imageConfig: ImageConfigRecord,
      onModeChange: Boolean => Unit,
      onStopClick: () => Unit,
      isEngineRunning: BooleanProperty
  )(using
      world: World,
      gameEngineRuntime: GameEngineRuntime
  ): Either[BaseError, VBox] =
    for
      panel <- delegate.build(imageConfig, onModeChange, onStopClick, isEngineRunning)
      performanceButton <- IconButton
        .build(
          PerformanceIcon(),
          IconButtonBaseProps(
            imageConfig = imageConfig,
            onClick = _ => onPerformanceExperiment(),
            isDisabled = isEngineRunning
          )
        )
        .map(MenuButton.styleIconButton)
        .left
        .map(error => CannotBuildPanel(error, PerformanceGameEngineModePanel.toString))
    yield
      panel.delegate.getChildren.asScala
        .collectFirst { case buttonsRow: JfxHBox =>
          buttonsRow
        }
        .foreach {
          _.getChildren.add(PerformanceButtonIndex, performanceButton.delegate)
        }
      panel
