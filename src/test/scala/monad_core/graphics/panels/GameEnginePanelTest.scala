package monad_core.graphics.panels

import monad_core.graphics.helpers.{MockImage, MockImageConfig}
import monad_core.graphics.panels.traits.{GameEngineModePanelBuilder, SceneRendererPanelBuilder}
import monad_core.graphics.resources.ImageConfigRecord
import monad_core.graphics.support.ScalaFxInit
import monad_core.graphics.{CannotBuildPanel, ImageResourceNotFound}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.layout.VBox

class GameEnginePanelTest extends AnyFunSuite with Inside with Matchers with MockFactory with ScalaFxInit:

  def correctMockedModePanel : GameEngineModePanelBuilder =
    val modePanel: GameEngineModePanelBuilder = mock[GameEngineModePanelBuilder]
    modePanel.build.expects(MockImageConfig()).returns(Right(new VBox {
      children = Seq()
    }))

    modePanel

  def invalidMockedModePanel: GameEngineModePanelBuilder =
    val modePanel: GameEngineModePanelBuilder = mock[GameEngineModePanelBuilder]
    modePanel.build.expects(MockImageConfig()).returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

    modePanel

  def correctMockedRendererPanel: SceneRendererPanelBuilder =
    val sceneRenderer: SceneRendererPanelBuilder = mock[SceneRendererPanelBuilder]
    (sceneRenderer.build _).expects().returns(Right(new VBox {
      children = Seq()
    }))

    sceneRenderer

  def invalidMockedRendererPanel: SceneRendererPanelBuilder =
    val rendererPanel: SceneRendererPanelBuilder = mock[SceneRendererPanelBuilder]
    (rendererPanel.build _).expects().returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

    rendererPanel

  def neverCalledRendererPanel: SceneRendererPanelBuilder =
    val rendererPanel: SceneRendererPanelBuilder = mock[SceneRendererPanelBuilder]
    (rendererPanel.build _).expects().never()

    rendererPanel

  test("A GameEnginePanel can be built"):
    val modePanel: GameEngineModePanelBuilder = correctMockedModePanel
    val rendererPanel: SceneRendererPanelBuilder = correctMockedRendererPanel
    val imageConfig: ImageConfigRecord = MockImageConfig()
    val enginePanel = GameEnginePanel(modePanel, rendererPanel, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Right(scene) =>
        scene shouldBe a[VBox]

  test("building with an invalid GameEngineModePanelBuilder returns an error"):
    val modePanel: GameEngineModePanelBuilder = invalidMockedModePanel
    val rendererPanel: SceneRendererPanelBuilder = neverCalledRendererPanel
    val imageConfig: ImageConfigRecord = MockImageConfig()
    val enginePanel = GameEnginePanel(modePanel, rendererPanel, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test("building with an invalid SceneRendererPanelBuilder returns an error"):
    val modePanel: GameEngineModePanelBuilder = correctMockedModePanel
    val rendererPanel: SceneRendererPanelBuilder = invalidMockedRendererPanel
    val imageConfig: ImageConfigRecord = MockImageConfig()
    val enginePanel = GameEnginePanel(modePanel, rendererPanel, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test("building with an invalid ImageConfigRecord returns an error"):
    val badImageConfig: ImageConfigRecord = mock[ImageConfigRecord]
    val modePanel: GameEngineModePanelBuilder = mock[GameEngineModePanelBuilder]
    val rendererPanel: SceneRendererPanelBuilder = neverCalledRendererPanel
    val enginePanel = GameEnginePanel(modePanel, rendererPanel, badImageConfig)
    modePanel.build.expects(badImageConfig).returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]