package integrations.monad_core.simulator.presentation.stages

import helpers.mocks.MockImage
import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import monad_core.engine.errors.EngineError
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.panels.traits.{AiModelChatPanelBuilder, GameEnginePanelBuilder}
import monad_core.simulator.presentation.stages.MainStage
import monad_core.simulator.{CannotBuildStage, ImageResourceNotFound}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.beans.property.DoubleProperty
import scalafx.scene.layout.{HBox, VBox}

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

class MainStageTest extends AnyFunSuite with Inside with Matchers with MockFactory with ScalaFxInit:
  given aiAgent: AiAgent = mock[AiAgent]

  val gamePanel: GameEnginePanelBuilder = mock[GameEnginePanelBuilder]
  val chatPanel: AiModelChatPanelBuilder = mock[AiModelChatPanelBuilder]

  val stageWidth: DoubleProperty = DoubleProperty(1000.0)
  val stageHeight: DoubleProperty = DoubleProperty(800.0)

  def setupCorrectGamePanel(): Unit =
    (gamePanel.build: () => Either[BaseError, VBox]).expects().returns(Right(new VBox {
      children = Seq()
    }))

  def setupInvalidGamePanel(): Unit =
    (gamePanel.build: () => Either[BaseError, VBox]).expects()
      .returns(Left(CannotBuildStage(ImageResourceNotFound(MockImage()), "")))

  def setupCorrectChatPanel(): Unit =
    (chatPanel.build(_: AiAgent)(using _: ExecutionContext)).expects(*, *)
      .returns(Right(new VBox {
        children = Seq()
      }))

  def setupInvalidChatPanel(): Unit =
    (chatPanel.build(_: AiAgent)(using _: ExecutionContext)).expects(*, *)
      .returns(Left(CannotBuildStage(ImageResourceNotFound(MockImage()), "")))

  def setupNeverCalledChatPanel(): Unit =
    (chatPanel.build(_: AiAgent)(using _: ExecutionContext)).expects(*, *).never()

  test("A MainStage can build its root content"):
    setupCorrectGamePanel()
    setupCorrectChatPanel()

    val mainStage = MainStage(gamePanel, chatPanel)
    val buildResult = mainStage.buildRootContent(stageWidth, stageHeight)

    inside(buildResult):
      case Right(root) =>
        root shouldBe a[HBox]
        root.children should have size 2

  test("building with an invalid GameEnginePanelBuilder returns an error and never calls the chat panel"):
    setupInvalidGamePanel()
    setupNeverCalledChatPanel()

    val mainStage = MainStage(gamePanel, chatPanel)
    val buildResult = mainStage.buildRootContent(stageWidth, stageHeight)

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildStage]

  test("building with an invalid AiModelChatPanelBuilder returns an error"):
    setupCorrectGamePanel()
    setupInvalidChatPanel()

    val mainStage = MainStage(gamePanel, chatPanel)
    val buildResult = mainStage.buildRootContent(stageWidth, stageHeight)

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildStage]

  test("the root content children order is chat panel then game panel"):
    val chatBox = new VBox {
      children = Seq()
    }
    val gameBox = new VBox {
      children = Seq()
    }
    (gamePanel.build: () => Either[BaseError, VBox]).expects().returns(Right(gameBox))
    (chatPanel.build(_: AiAgent)(using _: ExecutionContext)).expects(*, *)
      .returns(Right(chatBox))

    val mainStage = MainStage(gamePanel, chatPanel)
    val buildResult = mainStage.buildRootContent(stageWidth, stageHeight)

    inside(buildResult):
      case Right(root) =>
        root.children.head shouldBe chatBox.delegate
        root.children(1) shouldBe gameBox.delegate
