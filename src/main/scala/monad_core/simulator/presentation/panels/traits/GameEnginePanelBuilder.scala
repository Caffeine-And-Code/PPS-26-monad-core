package monad_core.simulator.presentation.panels.traits

import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

trait GameEnginePanelBuilder:
  def build(): Either[EngineError, VBox]