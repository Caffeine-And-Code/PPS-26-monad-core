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
 * GameEnginePanel concrete builder
 *
 * @param modePanel builder which creates the game engine controls, to enable the user to interact with the engine itself
 * @param rendererPanel builder which creates the view of the game engine state
 * @param imageConfig system configuration for the image usage
 * @param world current world
 * @param gameEngineRuntime the runtime that will be used to handle and mutate the provided world
 * @param painter the [[Painter]] which will be used to paint/display the elements of the world
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
     * The pure logic to handle the change of mode of the [[GameEngineRuntime]].
     *
     * It propagates the instruction, based on [[isButtonActive]], to the relative [[GameEngineRuntime]] function.
     *
     * @see [[GameEngineRuntime.stop()]] and [[GameEngineRuntime.start()]]
     * @param isButtonActive boolean value that symbolize the current state of the button of start and stop.
     *
     *                       if `true` is passed the engine is started, otherwise the engine is stopped
     */
    private def onModeChange(isButtonActive: Boolean): Unit =
      if isButtonActive then viewModel.gameEngineRuntime.start()
      else viewModel.gameEngineRuntime.stop()

      viewModel.isEngineRunning.value = viewModel.gameEngineRuntime.isRunning

    /**
     * The pure logic to handle the reset button click.
     *
     * The engine interaction is delegated to the [[GameEngineRuntime.resetToSnapshot()]] function.
     */
    private def onResetClick(): Unit =
      viewModel.gameEngineRuntime.resetToSnapshot()

  /**
   * Imperative shell that build the Panel itself.
   *
   * It first constructs each provided panel and then displays them in a static column layout:
   * - The mode panel as the first one occupying a [[TopPanelHeightRatio]] of the total height, with [[TopPanelMinHeight]] as minimum height
   * - The scene panel as the second one occupying the rest of the available space spaced by [[SpacingRatio]] from the top panel
   *
   * @see [[SceneRendererPanelBuilder]] and [[GameEngineModePanelBuilder]]
   * @return `Left(BaseError)` propagated by any of the panels build method
   *
   *         `Right(VBox)` if the panel is built, the [[VBox]] is the panel
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
