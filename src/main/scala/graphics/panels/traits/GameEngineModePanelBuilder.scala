package graphics.panels.traits

import engine.errors.EngineError
import graphics.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

trait GameEngineModePanelBuilder:
  def build()(using imageConfig: ImageConfigRecord): Either[EngineError, VBox]
