package monad_core.graphics.stages.traits

import monad_core.engine.errors.EngineError
import monad_core.graphics.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.graphics.resources.ImageConfigRecord
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.scene.layout.HBox

trait MainStageBuilder:
  def buildRootContent(
                        stageWidth: ReadOnlyDoubleProperty,
                        stageHeight: ReadOnlyDoubleProperty
                      ): Either[EngineError, HBox]
