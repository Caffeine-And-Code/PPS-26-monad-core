package monad_core.simulator.presentation.panels

import monad_core.engine.simulator.Painter
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.panels.traits.{
  GameEngineModePanelBuilder,
  GameEnginePanelBuilder,
  SceneRendererPanelBuilder
}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

/**
 * Composes the game-engine controls and scene renderer into a single panel.
 *
 * @param modePanel builder for the engine controls
 * @param rendererPanel builder for the current engine-state view
 * @param imageConfig image-loading configuration used by the controls
 * @param world world initialized and displayed by the panel
 * @param gameEngineRuntime runtime used to initialize and mutate the world
 * @param painter [[monad_core.engine.simulator.Painter Painter]] used to display world elements
 */
final class GameEnginePanel(
    modePanel: GameEngineModePanelBuilder,
    rendererPanel: SceneRendererPanelBuilder,
    imageConfig: ImageConfigRecord
)(using
    world: World,
    gameEngineRuntime: GameEngineRuntime,
    painter: Painter
) extends GameEnginePanelBuilder:

  private val TopPanelHeightRatio    = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio           = 0.02
  private val TopPanelMinHeight      = 80.0

  /**
   * ViewModel used as state of the [[GameEnginePanel]] component
   * @param gameEngineRuntime [[GameEngineRuntime]] used to handle the world
   */
  private case class GameEnginePanelViewModel(gameEngineRuntime: GameEngineRuntime):
    val isEngineRunning: BooleanProperty = BooleanProperty(gameEngineRuntime.isRunning)

  extension (viewModel: GameEnginePanelViewModel)

    /**
     * Starts or stops the runtime and synchronizes the observable running state.
     *
     * @param isButtonActive `true` to start the engine, or `false` to stop it
     */
    private def onModeChange(isButtonActive: Boolean): Unit =
      if isButtonActive then viewModel.gameEngineRuntime.start()
      else viewModel.gameEngineRuntime.stop()

      viewModel.isEngineRunning.value = viewModel.gameEngineRuntime.isRunning

    /** Restores the engine world to its current snapshot. */
    private def onResetClick(): Unit =
      viewModel.gameEngineRuntime.resetToSnapshot()

  /**
   * Initializes the runtime world, creates its baseline snapshot, and builds the panel.
   *
   * The controls occupy the top section, while the scene renderer fills the remaining
   * space in a responsive vertical layout.
   *
   * @see [[monad_core.simulator.presentation.panels.traits.SceneRendererPanelBuilder SceneRendererPanelBuilder]]
   * @see [[monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder GameEngineModePanelBuilder]]
   * @return `Left(CannotBuildPanel)` when either child builder fails, or `Right(VBox)` with the composed panel
   */
  def build(): Either[BaseError, VBox] =
    gameEngineRuntime.initializeWorld(world)
    gameEngineRuntime.createSnapshot()

    val viewModel = GameEnginePanelViewModel(gameEngineRuntime)

    for
      sceneRendererPanel <- rendererPanel
        .build(viewModel.isEngineRunning)
        .left
        .map(error => CannotBuildPanel(error, this.toString))

      gameEngineModePanel <- modePanel
        .build(
          imageConfig,
          viewModel.onModeChange,
          viewModel.onResetClick,
          viewModel.isEngineRunning
        )
        .left
        .map(error => CannotBuildPanel(error, this.toString))
    yield
      val container = new VBox {
        children = Seq(gameEngineModePanel, sceneRendererPanel)
      }
      container.spacing <== container.height * SpacingRatio

      gameEngineModePanel.prefHeight <== container.height * TopPanelHeightRatio
      gameEngineModePanel.minHeight = TopPanelMinHeight

      sceneRendererPanel.prefHeight <== container.height * BottomPanelHeightRatio

      container
