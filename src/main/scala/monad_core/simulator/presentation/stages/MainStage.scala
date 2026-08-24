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

/**
 * Imperative builder component that creates the MainStage of the application, containing the provided panels.
 * It will display them as side panels by giving them some padding between each other:
 * 1. `GameEnginePanelBuilder` is placed to the left of the window
 * 2. `AiModelChatPanelBuilder` is placed to the right of the window
 *
 * @see [[GameEnginePanelBuilder]], [[AiModelChatPanelBuilder]] and [[MainStageBuilder]]
 * @param gamePanel the build of the game engine panel
 * @param chatPanel the build of the ai chat panel
 */
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

  /**
   * Build the Main stage setting its width and height to the provided values.
   * It calls the two builders provided in the class constructor and displays them in a [[HBox]] spaced around by 
   * a calculated responsive padding.
   * 
   * @see [[bindResponsivePadding]] and [[assignPanelsSize]]
   * @param stageWidth the desired width of the stage 
   * @param stageHeight the desired height of the stage
   * @param aiAgent the agent required by the [[AiModelChatPanelBuilder]] 
   * @param executionContext the context of execution also required by the [[AiModelChatPanelBuilder]]
   * @return
   */
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

  /**
   * It calculates the padding based on the provided dimensions of the stage.
   * [[HorizontalPaddingRatio]] and [[VerticalPaddingRatio]] are used to find the actual padding value.
   * 
   * @param dimensions the stage desired size
   * @return [[Insets]] ScalaFx specific padding record 
   */
  private def computePadding(dimensions: StageDimensions): Insets =
    val h = dimensions.width.value * HorizontalPaddingRatio
    val v = dimensions.height.value * VerticalPaddingRatio
    
    Insets(v, h, v, h)

  /**
   * Binds the [[computePadding]] function to the `onWindowResize` event of ScalaFx.
   * It also sets the padding of the Main stage built Node to the calculated value.
   * 
   * @param rootContent the built Main stage 
   * @param dimensions the desired dimensions
   */
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

  /**
   * It calculates the panels actual dimensions based on the provided Main stage dimensions.
   * By doing so it also binds the values so that when the `onWindowResize` event occurs ([[bindResponsivePadding]]) the panels dimensions change accordingly. 
   * 
   * @see [[bindResponsivePadding]]
   * @param dimensions the desired Main stage dimensions
   * @param rootContent the built Main stage
   * @param leftPanel the built left panel  
   * @param rightPanel the built right panel
   * @return
   */
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
