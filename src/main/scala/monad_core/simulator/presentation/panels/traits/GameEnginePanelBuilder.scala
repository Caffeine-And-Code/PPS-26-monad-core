package monad_core.simulator.presentation.panels.traits

import monad_core.simulator.errors.BaseError
import scalafx.scene.layout.VBox

trait GameEnginePanelBuilder:
  def build(): Either[BaseError, VBox]