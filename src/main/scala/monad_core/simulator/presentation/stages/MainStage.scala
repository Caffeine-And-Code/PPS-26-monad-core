package monad_core.simulator.presentation.stages

import monad_core.simulator.CannotBuildStage
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.panels.traits.{
  AiModelChatPanelBuilder,
  GameEnginePanelBuilder
}
import monad_core.simulator.presentation.stages.traits.MainStageBuilder
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.geometry.Insets
import scalafx.scene.layout.{HBox, VBox}

import scala.concurrent.ExecutionContext

final class MainStage(
    gamePanel: GameEnginePanelBuilder,
    chatPanel: AiModelChatPanelBuilder
) extends MainStageBuilder:

  private val HorizontalPaddingRatio = 0.02
  private val VerticalPaddingRatio   = 0.02
  private val SpacingRatio           = 0.015
  private val LeftPanelWidthRatio    = 0.40
  private val RightPanelWidthRatio   = 0.58

  private case class StageDimensions(
      width: ReadOnlyDoubleProperty,
      height: ReadOnlyDoubleProperty
  )

  def buildRootContent(
      stageWidth: ReadOnlyDoubleProperty,
      stageHeight: ReadOnlyDoubleProperty
  )(using
      aiAgent: AiAgent,
      executionContext: ExecutionContext
  ): Either[BaseError, HBox] =
    val dimensions = StageDimensions(stageWidth, stageHeight)

    for
      builtGameEnginePanel <- gamePanel
        .build()
        .left
        .map(error => CannotBuildStage(error, this.toString))
      builtChatPanel <- chatPanel
        .build(aiAgent)
        .left
        .map(error => CannotBuildStage(error, this.toString))
    yield
      val rootContent = new HBox {
        children = Seq(builtChatPanel, builtGameEnginePanel)
      }
      bindResponsivePadding(rootContent, dimensions)
      assignPanelsSize(
        dimensions = dimensions,
        rootContent = rootContent,
        leftPanel = builtChatPanel,
        rightPanel = builtGameEnginePanel
      )

  private def computePadding(dimensions: StageDimensions): Insets =
    val h = dimensions.width.value * HorizontalPaddingRatio
    val v = dimensions.height.value * VerticalPaddingRatio
    Insets(v, h, v, h)

  private def bindResponsivePadding(
      rootContent: HBox,
      dimensions: StageDimensions
  ): Unit =
    def applyPadding(): Unit =
      rootContent.padding = computePadding(dimensions)

    dimensions.width.onChange(applyPadding())
    dimensions.height.onChange(applyPadding())

    // required for initial set up
    applyPadding()

  private def assignPanelsSize(
      dimensions: StageDimensions,
      rootContent: HBox,
      leftPanel: VBox,
      rightPanel: VBox
  ): HBox =
    val calculateInvertPercentage: Double => Double =
      (paddingRatio: Double) => 1.0 - 2 * paddingRatio

    val availableWidth  = dimensions.width * calculateInvertPercentage(HorizontalPaddingRatio)
    val availableHeight = dimensions.height * calculateInvertPercentage(VerticalPaddingRatio)

    rootContent.spacing <== availableWidth * SpacingRatio
    leftPanel.prefWidth <== availableWidth * LeftPanelWidthRatio
    rightPanel.prefWidth <== availableWidth * RightPanelWidthRatio
    leftPanel.prefHeight <== availableHeight
    rightPanel.prefHeight <== availableHeight

    rootContent
