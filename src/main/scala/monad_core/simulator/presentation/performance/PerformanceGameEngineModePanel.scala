package monad_core.simulator.presentation.performance

import javafx.scene.layout.HBox as JfxHBox
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.performance.simulator.PerformanceCli
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.{
  Error,
  IconButton,
  IconButtonBaseProps,
  MenuButton,
  NotificationManager
}
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.resources.Image.PerformanceIcon
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

/**
 * Adds the optional performance control to an existing engine-mode panel builder.
 *
 * @param delegate
 *   base panel builder whose controls and behavior are preserved
 * @see
 *   [[monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder GameEngineModePanelBuilder]]
 */
final case class PerformanceGameEngineModePanel(
    delegate: GameEngineModePanelBuilder
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
            onClick = _ => openExperiment(gameEngineRuntime),
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

  /**
   * Opens the experiment dialog and reports an unexpected graphical failure.
   *
   * @param gameEngineRuntime runtime providing the currently enabled physics rules
   * @see
   *   [[monad_core.simulator.presentation.performance.ExperimentDialog.show ExperimentDialog.show]]
   */
  private def openExperiment(gameEngineRuntime: GameEngineRuntime): Unit =
    ExperimentDialog
      .show(runExperiment(gameEngineRuntime))
      .left
      .foreach(error => NotificationManager.show(error.message, Error))

  /**
   * Creates an asynchronous experiment using the runtime's current rule configuration.
   *
   * @param gameEngineRuntime runtime providing the currently enabled physics rules
   * @return asynchronous operation accepted by the experiment dialog
   * @see
   *   [[monad_core.performance.simulator.PerformanceCli.runWithRules PerformanceCli.runWithRules]]
   */
  private def runExperiment(
      gameEngineRuntime: GameEngineRuntime
  ): ExperimentDialog.RunExperiment = command =>
    val rules = gameEngineRuntime.physicsRules
    Future {
      PerformanceCli.runWithRules(command.route, command.arguments.toArray, rules)
    }(ExecutionContext.global)
