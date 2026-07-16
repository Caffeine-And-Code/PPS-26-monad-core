package monad_core.simulator.presentation.stages.traits

import monad_core.engine.errors.EngineError
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.scene.layout.HBox

trait MainStageBuilder:
  def buildRootContent(
                        stageWidth: ReadOnlyDoubleProperty,
                        stageHeight: ReadOnlyDoubleProperty
                      ): Either[EngineError, HBox]
