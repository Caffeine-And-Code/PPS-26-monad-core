package graphics.panels.traits

import engine.errors.EngineError
import scalafx.scene.layout.VBox

trait SceneRendererPanelBuilder :
  def build(): Either[EngineError, VBox]
