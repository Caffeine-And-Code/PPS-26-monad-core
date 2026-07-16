package graphics.stages

import engine.errors.EngineError
import graphics.CannotBuildStage
import graphics.panels.traits.{AiModelChatPanelBuilder, GameEngineModePanelBuilder, GameEnginePanelBuilder, SceneRendererPanelBuilder}
import graphics.panels.{AiModelChatPanel, GameEnginePanel}
import graphics.resources.ImageConfigRecord
import graphics.stages.traits.MainStageBuilder
import scalafx.application.Platform
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.stage.Stage

import java.util.concurrent.{CountDownLatch, TimeUnit}

object MainStage extends MainStageBuilder {

  private val HorizontalPaddingRatio = 0.02
  private val VerticalPaddingRatio = 0.02
  private val SpacingRatio = 0.015
  private val LeftPanelWidthRatio = 0.40
  private val RightPanelWidthRatio = 0.58

  private val MinStageWidth = 1024.0
  private val MinStageHeight = 720.0

  def main()
          (
            using imageConfig: ImageConfigRecord,
            gameEnginePanelBuilder: GameEnginePanelBuilder,
            aiModelChatPanelBuilder: AiModelChatPanelBuilder,
            gameEngineModePanelBuilder: GameEngineModePanelBuilder,
            sceneRendererPanelBuilder: SceneRendererPanelBuilder
          )
  : Option[EngineError] =
    val latch = new CountDownLatch(1)
    @volatile var startupError: Option[EngineError] = None

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

      buildRootContent(mainScene.width, mainScene.height) match
        case Right(rootContent) =>
          mainScene.content = rootContent
          mainStage.scene = mainScene
          mainStage.show()

        case Left(error) =>
          startupError = Some(error)

      latch.countDown()
    })

    // wait for setup to finish
    latch.await(10, TimeUnit.SECONDS)
    startupError

  private def buildRootContent(
                                stageWidth: ReadOnlyDoubleProperty,
                                stageHeight: ReadOnlyDoubleProperty
                              )
                              (
                                using imageConfig: ImageConfigRecord,
                                gameEnginePanelBuilder: GameEnginePanelBuilder,
                                aiModelChatPanelBuilder: AiModelChatPanelBuilder,
                                gameEngineModePanelBuilder: GameEngineModePanelBuilder,
                                sceneRendererPanelBuilder: SceneRendererPanelBuilder
                              ): Either[EngineError, HBox] =
    val gameEnginePanelEither = gameEnginePanelBuilder.build()
    val modelChatPanelEither = aiModelChatPanelBuilder.build()

    (gameEnginePanelEither, modelChatPanelEither) match
      case (Right(gameEnginePanel), Right(modelChatPanel)) =>
        val rootContent = new HBox {
          children = Seq(modelChatPanel, gameEnginePanel)
        }

        bindResponsivePadding(rootContent, stageWidth, stageHeight)

        Right(
          assignPanelsSize(
            stageWidth = stageWidth,
            stageHeight = stageHeight,
            rootContent = rootContent,
            leftPanel = modelChatPanel,
            rightPanel = gameEnginePanel
          )
        )

      case (Left(error), _) => Left(CannotBuildStage(error, MainStage.toString))
      case (_, Left(error)) => Left(CannotBuildStage(error, MainStage.toString))


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