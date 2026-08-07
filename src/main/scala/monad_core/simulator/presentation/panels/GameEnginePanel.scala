package monad_core.simulator.presentation.panels

import monad_core.engine.public_api.Painter
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.{GameEngineRuntime, ShapeArchitect}
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.panels.traits.{GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

final class GameEnginePanel(
                             modePanel: GameEngineModePanelBuilder,
                             rendererPanel: SceneRendererPanelBuilder,
                             imageConfig: ImageConfigRecord
                           )(
                             using world: World,
                             gameEngineRuntime: GameEngineRuntime,
                             architect: ShapeArchitect,
                             painter: Painter
                           ) extends GameEnginePanelBuilder :

  private val TopPanelHeightRatio = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio = 0.02
  private val TopPanelMinHeight = 80.0

  def build()
  : Either[BaseError, VBox] = 
    gameEngineRuntime.initializeWorld(world)
    gameEngineRuntime.createSnapshot()

    val isEngineRunning = BooleanProperty(gameEngineRuntime.isRunning)

    for
      sceneRendererPanel <- rendererPanel.build()
        .left.map(error => CannotBuildPanel(error, this.toString))

      onModeChange = (isButtonActive: Boolean) =>
        if isButtonActive then gameEngineRuntime.start()
        else gameEngineRuntime.stop()
        isEngineRunning.value = gameEngineRuntime.isRunning

      onStopClick = () => gameEngineRuntime.resetToSnapshot()

      gameEngineModePanel <- modePanel.build(imageConfig, onModeChange, onStopClick, isEngineRunning)
        .left.map(error => CannotBuildPanel(error, this.toString))
    yield
      val container = new VBox {
        children = Seq(gameEngineModePanel, sceneRendererPanel)
      }

      container.spacing <== container.height * SpacingRatio

      gameEngineModePanel.prefHeight <== container.height * TopPanelHeightRatio
      gameEngineModePanel.minHeight = TopPanelMinHeight

      sceneRendererPanel.prefHeight <== container.height * BottomPanelHeightRatio

      container
  