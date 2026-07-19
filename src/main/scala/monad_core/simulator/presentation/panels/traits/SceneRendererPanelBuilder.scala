package monad_core.simulator.presentation.panels.traits

import monad_core.engine.core.GameLoop
import monad_core.engine.errors.EngineError
import scalafx.scene.layout.VBox

trait SceneRendererPanelBuilder :
  def build(gameLoop: GameLoop): Either[EngineError, VBox]
