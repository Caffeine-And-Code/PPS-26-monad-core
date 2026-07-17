package integrations.monad_core.simulator.presentation.panels

import helpers.{MockImage, MockImageConfig}
import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import monad_core.engine.errors.EngineError
import monad_core.simulator.presentation.panels.GameEnginePanel
import monad_core.simulator.presentation.panels.traits.{GameEngineModePanelBuilder, SceneRendererPanelBuilder}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import monad_core.simulator.{CannotBuildPanel, ImageResourceNotFound}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.layout.VBox

class GameEnginePanelTest extends AnyFunSuite with Inside with Matchers with MockFactory with ScalaFxInit:
  val modePanel: GameEngineModePanelBuilder = mock[GameEngineModePanelBuilder]
  val sceneRenderer: SceneRendererPanelBuilder = mock[SceneRendererPanelBuilder]
  val imageConfig: ImageConfigRecord = MockImageConfig()

  def setupCorrectModePanel(): Unit =
    modePanel.build.expects(MockImageConfig()).returns(Right(new VBox {
      children = Seq()
    }))

  def setupInvaliModePanel(): Unit =
    modePanel.build.expects(MockImageConfig()).returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

  def setupCorrectSceneRenderer(): Unit =
    (sceneRenderer.build: () => Either[EngineError, VBox]).expects().returns(Right(new VBox {
      children = Seq()
    }))

  def setupInvalidSceneRenderer(): Unit =
    (sceneRenderer.build: () => Either[EngineError, VBox]).expects().returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

  def setupNeverCalledSceneRenderer(): Unit =
    (sceneRenderer.build: () => Either[EngineError, VBox]).expects().never()

  test("A GameEnginePanel can be built"):
    setupCorrectModePanel()
    setupCorrectSceneRenderer()
    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Right(scene) =>
        scene shouldBe a[VBox]

  test("building with an invalid GameEngineModePanelBuilder returns an error"):
    setupInvaliModePanel()
    setupNeverCalledSceneRenderer()
    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test("building with an invalid SceneRendererPanelBuilder returns an error"):
    setupCorrectModePanel()
    setupInvalidSceneRenderer()
    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test("building with an invalid ImageConfigRecord returns an error"):
    setupNeverCalledSceneRenderer()
    val badImageConfig: ImageConfigRecord = mock[ImageConfigRecord]
    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, badImageConfig)
    modePanel.build.expects(badImageConfig).returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]