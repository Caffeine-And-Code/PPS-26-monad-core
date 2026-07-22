package monad_core.simulator.presentation.panels

import monad_core.engine.core.*
import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.{GameEngineRuntime, SaveEntityCommand, World}
import monad_core.simulator.presentation.panels.traits.{GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

final class GameEnginePanel(
                             modePanel: GameEngineModePanelBuilder,
                             rendererPanel: SceneRendererPanelBuilder,
                             imageConfig: ImageConfigRecord
                           ) extends GameEnginePanelBuilder {

  private val TopPanelHeightRatio = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio = 0.02
  private val TopPanelMinHeight = 80.0

  def build()
           (
             using gameEngineRuntime: GameEngineRuntime,
             world: World
           )
  : Either[EngineError, VBox] = {
    val gameEngine = initialSetup(world, gameEngineRuntime).fold(error => return Left(error), gameEngineRuntime => gameEngineRuntime)

    val onModeChange: Boolean => Unit =
      isButtonActive =>
        if isButtonActive then
          gameEngine.stop()
        else {
          gameEngineRuntime.start()
        }

    val onStopClick: () => Unit = () => gameEngine.stop()

    for
      gameEngineModePanel <- modePanel.build(imageConfig, onModeChange, onStopClick)
        .left.map(error => CannotBuildPanel(error, this.toString))
      sceneRendererPanel <- rendererPanel.build()
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
  }

  private[panels] def initialSetup(world: World, gameEngineRuntime: GameEngineRuntime): Either[EngineError, GameEngineRuntime] =
    val entity = Entity.circle("starter", Vector2D(0, 0), 5)
      .fold(error => return Left(error), entity => entity)

    val initialWorld = world.createEntity(SaveEntityCommand(entity))
      .fold(error => return Left(error), world => world)

    gameEngineRuntime.init(
      world,
      world => world
    )

}