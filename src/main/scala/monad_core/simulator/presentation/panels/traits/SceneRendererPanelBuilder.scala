package monad_core.simulator.presentation.panels.traits

import monad_core.engine.errors.EngineError
import monad_core.simulator.application.engine.GameEngine
import monad_core.simulator.application.engine.world.World
import scalafx.scene.layout.VBox

trait SceneRendererPanelBuilder:
  def build(world: World): Either[EngineError, (VBox, GameEngine)]