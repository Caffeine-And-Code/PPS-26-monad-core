package integrations.monad_core.simulator.presentation.panels

import helpers.mocks.{MockImage, MockImageConfig}
import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import monad_core.engine.model.Scene
import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.application.engine.{GameEngineRuntime, ShapeArchitect}
import monad_core.simulator.infrastructure.engine.painters.PaintArchitect
import monad_core.simulator.infrastructure.engine.{MonadCoreGameEngineRuntime, MonadCoreWorld}
import monad_core.simulator.presentation.panels.GameEnginePanel
import monad_core.simulator.presentation.panels.traits.{
  GameEngineModePanelBuilder,
  SceneRendererPanelBuilder
}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import monad_core.simulator.{CannotBuildPanel, ImageResourceNotFound}
import org.scalamock.function.{MockFunction0, MockFunction1}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

class GameEnginePanelTest
    extends AnyFunSuite
    with Inside
    with Matchers
    with MockFactory
    with ScalaFxInit:
  given MonadCoreGameEngineRuntime = MonadCoreGameEngineRuntime()

  given painter: Painter          = PaintArchitect
  given architect: ShapeArchitect = PaintArchitect

  given World = MonadCoreWorld()

  val modePanel: GameEngineModePanelBuilder      = mock[GameEngineModePanelBuilder]
  val sceneRenderer: SceneRendererPanelBuilder   = mock[SceneRendererPanelBuilder]
  val imageConfig: ImageConfigRecord             = MockImageConfig()
  val onModeChange: MockFunction1[Boolean, Unit] = mockFunction[Boolean, Unit]
  val onStopClick: MockFunction0[Unit]           = mockFunction[Unit]

  override def beforeAll(): Unit =
    onModeChange.expects(*).never()
    onStopClick.expects().never()

  def setupCorrectModePanel(): Unit =
    (modePanel
      .build(_: ImageConfigRecord, _: Boolean => Unit, _: () => Unit, _: BooleanProperty)(using
        _: World,
        _: GameEngineRuntime
      ))
      .expects(*, *, *, *, *, *)
      .returns(
        Right(
          new VBox {
            children = Seq()
          }
        )
      )

  def setupInvalidModePanel(): Unit =
    (modePanel
      .build(_: ImageConfigRecord, _: Boolean => Unit, _: () => Unit, _: BooleanProperty)(using
        _: World,
        _: GameEngineRuntime
      ))
      .expects(*, *, *, *, *, *)
      .returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

  def setupNeverCalledModePanel(): Unit =
    (modePanel
      .build(_: ImageConfigRecord, _: Boolean => Unit, _: () => Unit, _: BooleanProperty)(using
        _: World,
        _: GameEngineRuntime
      ))
      .expects(*, *, *, *, *, *)
      .never()

  def setupCorrectSceneRenderer(): Unit =
    (sceneRenderer
      .build(_: BooleanProperty)(using
        _: GameEngineRuntime,
        _: World,
        _: ShapeArchitect,
        _: Painter
      ))
      .expects(*, *, *, *, *)
      .returns(
        Right(
          new VBox {
            children = Seq()
          }
        )
      )

  def setupInvalidSceneRenderer(): Unit =
    (sceneRenderer
      .build(_: BooleanProperty)(using
        _: GameEngineRuntime,
        _: World,
        _: ShapeArchitect,
        _: Painter
      ))
      .expects(*, *, *, *, *)
      .returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

  def setupNeverCalledSceneRenderer(): Unit =
    (sceneRenderer
      .build(_: BooleanProperty)(using
        _: GameEngineRuntime,
        _: World,
        _: ShapeArchitect,
        _: Painter
      ))
      .expects(*, *, *, *, *)
      .never()

  test("A GameEnginePanel can be built"):
    setupCorrectSceneRenderer()
    setupCorrectModePanel()
    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Right(scene) =>
        scene shouldBe a[VBox]

  test("building with an invalid GameEngineModePanelBuilder returns an error"):
    // SceneRenderer runs first and succeeds, then ModePanel runs and fails
    setupCorrectSceneRenderer()
    setupInvalidModePanel()
    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test("building with an invalid SceneRendererPanelBuilder returns an error"):
    // SceneRenderer runs first and fails; ModePanel is short-circuited and never called
    setupInvalidSceneRenderer()
    setupNeverCalledModePanel()
    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, imageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test("building with an invalid ImageConfigRecord returns an error"):
    // SceneRenderer runs first and succeeds, then ModePanel receives bad config and fails
    setupCorrectSceneRenderer()
    val badImageConfig: ImageConfigRecord = mock[ImageConfigRecord]

    (modePanel
      .build(_: ImageConfigRecord, _: Boolean => Unit, _: () => Unit, _: BooleanProperty)(using
        _: World,
        _: GameEngineRuntime
      ))
      .expects(badImageConfig, *, *, *, *, *)
      .returns(Left(CannotBuildPanel(ImageResourceNotFound(MockImage()), "")))

    val enginePanel = GameEnginePanel(modePanel, sceneRenderer, badImageConfig)

    val buildResult = enginePanel.build()

    inside(buildResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]
