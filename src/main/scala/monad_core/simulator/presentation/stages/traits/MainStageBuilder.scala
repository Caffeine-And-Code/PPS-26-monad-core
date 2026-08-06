package monad_core.simulator.presentation.stages.traits

import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.errors.BaseError
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.scene.layout.HBox

import scala.concurrent.ExecutionContext

trait MainStageBuilder:
  def buildRootContent(
                        stageWidth: ReadOnlyDoubleProperty,
                        stageHeight: ReadOnlyDoubleProperty
                      )
                      (
                      using             
                      aiAgent: AiAgent,
                       executionContext: ExecutionContext
                      ): Either[BaseError, HBox]
