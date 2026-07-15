package graphics.stages

import graphics.panels.{AiModelChatPanel, GameEnginePanel}
import scalafx.application.Platform
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Stage

object MainStage {

  private val HorizontalPaddingRatio = 0.02
  private val VerticalPaddingRatio = 0.02
  private val SpacingRatio = 0.015
  private val LeftPanelWidthRatio = 0.40
  private val RightPanelWidthRatio = 0.58

  private val MinStageWidth = 1024.0
  private val MinStageHeight = 720.0

  def main(args: Array[String]): Unit = {

    Platform.startup(() => {
      val mainStage = new Stage {
        title = "MonadCore2D"
        fullScreen = true
        minWidth = MinStageWidth
        minHeight = MinStageHeight
      }

      val mainScene = new Scene(900, 600) {
        fill = Color.rgb(25, 26, 28)
      }

      val rootContent = buildRootContent(mainScene.width, mainScene.height)

      mainScene.content = rootContent
      mainStage.scene = mainScene

      mainStage.show()
    })
  }

  private def buildRootContent(
                                stageWidth: ReadOnlyDoubleProperty,
                                stageHeight: ReadOnlyDoubleProperty
                              ): HBox =
    val sceneDrawerPanel = GameEnginePanel.build()
    val modelChatPanel = AiModelChatPanel.build()

    val rootContent = new HBox {
      children = Seq(modelChatPanel, sceneDrawerPanel)
    }

    bindResponsivePadding(rootContent, stageWidth, stageHeight)

    assignPanelsSize(
      stageWidth = stageWidth,
      stageHeight = stageHeight,
      rootContent = rootContent,
      leftPanel = modelChatPanel,
      rightPanel = sceneDrawerPanel
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