package monad_core.graphics.panels.traits

import monad_core.engine.errors.EngineError
import scalafx.scene.layout.VBox

trait SceneRendererPanelBuilder :
  def build(): Either[EngineError, VBox]
