package graphics.stages

import graphics.panels.{ModelChat, SceneDrawer}
import graphics.stages.support.{-, Padding, Size, toInsets}
import scalafx.application.Platform
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.scene.Scene
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Stage

object MainStage {
  def main(args: Array[String]): Unit = {

    Platform.startup(() => {
      val mainStage = new Stage {
        title = "MonadCore2D"
        fullScreen = true
      }

      val mainScene = new Scene(900, 600) {
        fill = Color.rgb(25, 26, 28)
      }

      val mainStageSize = Size(mainScene.width, mainScene.height)

      val rootContent = buildRootContent(mainStageSize)

      mainScene.content = rootContent
      mainStage.scene = mainScene

      mainStage.show()
    })
  }

  private def buildRootContent(stageSize: Size[ReadOnlyDoubleProperty]): HBox =
    val verticalSpacing = 20
    val horizontalSpacing = 50
    val stagePadding = Padding.symmetrical(horizontalSpacing, verticalSpacing)

    val sceneDrawerPanel = SceneDrawer.build()
    val modelChatPanel = ModelChat.build()

    val rootContent = new HBox {
      padding = stagePadding.toInsets
      children = Seq(modelChatPanel, sceneDrawerPanel)
    }

    assignPanelsSize(
      stagePadding = stagePadding,
      stageSize = stageSize,
      rootContent = rootContent,
      leftPanel = modelChatPanel,
      rightPanel = sceneDrawerPanel
    )

  private def assignPanelsSize(
                                stagePadding: Padding,
                                stageSize: Size[ReadOnlyDoubleProperty],
                                rootContent: HBox,
                                leftPanel: VBox,
                                rightPanel: VBox
                              ): HBox =
    val (availableWidth, availableHeight) = stageSize - stagePadding

    rootContent.spacing <== availableWidth * 0.02

    leftPanel.prefWidth <== availableWidth * 0.40
    rightPanel.prefWidth <== availableWidth * 0.58

    leftPanel.prefHeight <== availableHeight
    rightPanel.prefHeight <== availableHeight

    rootContent
}