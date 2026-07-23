package monad_core.simulator.presentation.panels

import monad_core.engine.core.Scene
import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
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

  def build(): Either[EngineError, VBox] = {

    val initialWorld = buildInitialWorld(World(Scene()))
      .fold(error => return Left(error), w => w)

    for
      (sceneRendererPanel, controller) <- rendererPanel.build(initialWorld)
        .left.map(error => CannotBuildPanel(error, this.toString))

      onModeChange = (isButtonActive: Boolean) =>
        if isButtonActive then controller.play()
        else controller.pause()

      onStopClick = () => controller.init(initialWorld)

      gameEngineModePanel <- modePanel.build(imageConfig, onModeChange, onStopClick)
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

  private[panels] def buildInitialWorld(world: World): Either[EngineError, World] =
    for
      entity        <- Entity.circle("starter", Vector2D(10, 10), 5)
      updatedWorld  <- world.createEntity(SaveEntityCommand(entity))
    yield updatedWorld
}