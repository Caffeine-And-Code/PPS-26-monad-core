package monad_core.simulator.presentation.panels

import monad_core.engine.core.Scene
import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, World}
import monad_core.simulator.infrastructure.engine.MonadCoreWorld
import monad_core.simulator.presentation.panels.traits.{GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

final class GameEnginePanel(
                             modePanel: GameEngineModePanelBuilder,
                             rendererPanel: SceneRendererPanelBuilder,
                             imageConfig: ImageConfigRecord
                           )(
                             using world: World,
                             gameEngineRuntime: GameEngineRuntime
                           ) extends GameEnginePanelBuilder {

  private val TopPanelHeightRatio = 0.07
  private val BottomPanelHeightRatio = 0.93
  private val SpacingRatio = 0.02
  private val TopPanelMinHeight = 80.0

  def build()
  : Either[EngineError, VBox] = {
    buildInitialWorld(world)
    val worldSnapshot: Option[Scene] = Some(world.scene)

    for
      sceneRendererPanel <- rendererPanel.build()
        .left.map(error => CannotBuildPanel(error, this.toString))

      onModeChange = (isButtonActive: Boolean) =>
        if isButtonActive then gameEngineRuntime.start()
        else gameEngineRuntime.stop()

      onStopClick = () => gameEngineRuntime.reset(MonadCoreWorld(worldSnapshot.get))

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

  private[panels] def buildInitialWorld(world: World): Either[EngineError, Unit] =
    Entity.circle("starter", Vector2D(15, 15), 15).flatMap(
      entity => world.createEntity(SaveEntityCommand(entity))
    )
}