package monad_core.graphics.panels.traits

import monad_core.engine.errors.EngineError
import monad_core.graphics.resources.ImageConfigRecord
import scalafx.scene.layout.VBox

trait GameEnginePanelBuilder:
  def build()
           (
             using imageConfig: ImageConfigRecord,
             gameEngineModePanelBuilder: GameEngineModePanelBuilder,
             sceneRendererPanelBuilder: SceneRendererPanelBuilder
           )
  : Either[EngineError, VBox]
