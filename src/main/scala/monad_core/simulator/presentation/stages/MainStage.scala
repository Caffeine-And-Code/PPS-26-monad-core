package monad_core.simulator.presentation.stages

import monad_core.engine.errors.EngineError
import monad_core.simulator.CannotBuildStage
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.NotificationManager
import monad_core.simulator.presentation.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import monad_core.simulator.presentation.stages.traits.MainStageBuilder
import scalafx.application.Platform
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.geometry.Insets
import scalafx.scene.layout.{HBox, StackPane, VBox}

import scala.concurrent.ExecutionContext

final class MainStage(
                       gamePanel: GameEnginePanelBuilder,
                       chatPanel: AiModelChatPanelBuilder
                     ) extends MainStageBuilder {

  private val HorizontalPaddingRatio = 0.02
  private val VerticalPaddingRatio = 0.02
  private val SpacingRatio = 0.015
  private val LeftPanelWidthRatio = 0.40
  private val RightPanelWidthRatio = 0.58

  def buildRootContent(
                        stageWidth: ReadOnlyDoubleProperty,
                        stageHeight: ReadOnlyDoubleProperty
                      )
                      (
                        using
                        aiAgent: AiAgent,
                        executionContext: ExecutionContext
                      ): Either[BaseError, HBox] =
    for
      builtGameEnginePanel <- gamePanel.build()
        .left.map(error => CannotBuildStage(error, this.toString))
      builtChatPanel <- chatPanel.build(aiAgent)
        .left.map(error => CannotBuildStage(error, this.toString))
    yield
      val rootContent = new HBox {
        children = Seq(builtChatPanel, builtGameEnginePanel)
      }

      bindResponsivePadding(rootContent, stageWidth, stageHeight)

      assignPanelsSize(
        stageWidth = stageWidth,
        stageHeight = stageHeight,
        rootContent = rootContent,
        leftPanel = builtChatPanel,
        rightPanel = builtGameEnginePanel
      )

  private def bindResponsivePadding(
                                     rootContent: HBox,
                                     stageWidth: ReadOnlyDoubleProperty,
                                     stageHeight: ReadOnlyDoubleProperty
                                   ): Unit =
    def updatePadding(): Unit =
      val h = stageWidth.value * HorizontalPaddingRatio
      val v = stageHeight.value * VerticalPaddingRatio
      rootContent.padding = Insets(v, h, v, h)

    stageWidth.onChange {
      updatePadding()
    }
    stageHeight.onChange {
      updatePadding()
    }
    // required for initial set up
    updatePadding()

  private def assignPanelsSize(
                                stageWidth: ReadOnlyDoubleProperty,
                                stageHeight: ReadOnlyDoubleProperty,
                                rootContent: HBox,
                                leftPanel: VBox,
                                rightPanel: VBox
                              ): HBox =
    val calculateInvertPercentage: Double => Double =
      (paddingRatio: Double) => 1.0 - 2 * paddingRatio

    val availableWidth = stageWidth * calculateInvertPercentage(HorizontalPaddingRatio)
    val availableHeight = stageHeight * calculateInvertPercentage(VerticalPaddingRatio)

    rootContent.spacing <== availableWidth * SpacingRatio

    leftPanel.prefWidth <== availableWidth * LeftPanelWidthRatio
    rightPanel.prefWidth <== availableWidth * RightPanelWidthRatio

    leftPanel.prefHeight <== availableHeight
    rightPanel.prefHeight <== availableHeight

    rootContent
}
