package graphics.panels.traits

import engine.errors.EngineError
import graphics.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

trait GameEnginePanelBuilder:
  def build()
           (
             using imageConfig: ImageConfigRecord,
             gameEngineModePanelBuilder: GameEngineModePanelBuilder,
             sceneRendererPanelBuilder: SceneRendererPanelBuilder
           )
  : Either[EngineError, VBox]
