package monad_core.simulator.presentation.stages.traits

import dev.langchain4j.service.AiServices
import monad_core.engine.errors.EngineError
import monad_core.simulator.application.ai.AiAgent
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
                      ): Either[EngineError, HBox]
