package integrations.monad_core.simulator.presentation.panels

import helpers.mocks.MockImageConfig
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import javafx.scene.control.Button
import javafx.scene.layout.HBox
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.MonadCoreGameEngineRuntime
import monad_core.simulator.presentation.panels.GameEngineModePanel
import monad_core.simulator.presentation.resources.ImageConfigRecord
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import scalafx.Includes.{jfxButton2sfx, jfxHBox2sfx}
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

class GameEngineModePanelTest
    extends AnyFunSuite
    with Inside
    with Matchers
    with MockFactory
    with ScalaFxInit:

  given mockWorld: World                     = mock[World]
  given gameEngineRuntime: GameEngineRuntime = MonadCoreGameEngineRuntime()

  private val ModePanel              = GameEngineModePanel
  private val ToolsButtonIndex       = 0
  private val PhysicsButtonIndex     = 1
  private val PerformanceButtonIndex = 2
  private val SpacingRegionIndex     = 3
  private val ModeButtonIndex        = 4
  private val StopButtonIndex        = 5
  private val ImageConfigRecord      = MockImageConfig()
  private val OnModeChange           = mockFunction[Boolean, Unit]
  private val OnStopClick            = mockFunction[Unit]

  private def FreshSceneCanBeUpdated: BooleanProperty = BooleanProperty(false)

  private def FreshSceneCannotBeUpdated: BooleanProperty = BooleanProperty(true)

  test("A GameEngineModePanel can be created"):

    val builderResult: Either[BaseError, VBox] = ModePanel.build(
      ImageConfigRecord,
      OnModeChange,
      OnStopClick,
      FreshSceneCanBeUpdated
    )

    inside(builderResult):
      case Right(scene) =>
        scene.children.getFirst shouldBe a[HBox]

  test("A GameEngineModePanel should contain the physics rules menu button"):
    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        buttonsRow.children.get(PhysicsButtonIndex) shouldBe a[Button]

  test("A GameEngineModePanel should contain the performance-test button"):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        buttonsRow.children.get(PerformanceButtonIndex) shouldBe a[Button]

  test(
    "A GameEngineModePanel should run its configured action when the performance-test button is clicked"
  ):
    var performanceTestRuns = 0
    val configuredModePanel =
      GameEngineModePanel.withPerformanceExperiment(() => performanceTestRuns += 1)

    val builderResult = getOrFail(
      configuredModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    onFxThread {
      val performanceButton = builderResult.delegate.getChildren.getFirst
        .asInstanceOf[HBox]
        .getChildren
        .get(PerformanceButtonIndex)
        .asInstanceOf[Button]

      performanceButton.fire()

      performanceTestRuns shouldBe 1
    }

  test("A GameEngineModePanel cannot be built when an invalid image config record is passed"):
    val invalidImageConfig: ImageConfigRecord = mock[ImageConfigRecord]

    val builderResult = ModePanel.build(
      invalidImageConfig,
      OnModeChange,
      OnStopClick,
      FreshSceneCanBeUpdated
    )

    inside(builderResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test(
    "Mode Button Click event calls the passed onModeChange function with isActive equal to true after one click"
  ):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    OnModeChange.expects(true).once()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn)

  test(
    "Mode Button Click event calls the passed onModeChange function with isActive equal to false after two clicks"
  ):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    inSequence:
      OnModeChange.expects(true).once()
      OnModeChange.expects(false).once()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn, times = 2)

  test("Stop Button Click cannot be clicked upon scene start up when the isEngineRunning is false"):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    OnStopClick.expects().never()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click can be clicked upon scene start up when the isEngineRunning is true"):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCannotBeUpdated
      )
    )

    OnStopClick.expects().once()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click can be clicked once the PlayButton is clicked once"):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    OnModeChange.expects(true)
    OnStopClick.expects()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn)
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click cannot be clicked once the PlayButton is clicked two times"):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    inSequence:
      OnModeChange.expects(true)
      OnModeChange.expects(false)

    OnStopClick.expects().never()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn, times = 2)
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click can be clicked when the PlayButton is active"):

    val builderResult = getOrFail(
      ModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCanBeUpdated
      )
    )

    inSequence:
      OnModeChange.expects(true)
      OnModeChange.expects(false)
      OnModeChange.expects(true)

    OnStopClick.expects()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn, times = 3)
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test(
    "Tools Button cannot be clicked once the GameEngine is running and can be clicked when the engine is not running"
  ):
    val cases = Table(
      ("isEngineRunning", "expectedIsDisableValue"),
      (FreshSceneCannotBeUpdated, true),
      (FreshSceneCanBeUpdated, false)
    )

    forAll(cases): (isEngineRunning, expectedIsDisableValue) =>

      val builderResult = getOrFail(
        ModePanel.build(ImageConfigRecord, OnModeChange, OnStopClick, isEngineRunning)
      )

      OnModeChange.expects(*).never()
      OnStopClick.expects().never()

      inside(builderResult.children.head):
        case buttonsRow: HBox =>
          inside(buttonsRow.children.get(ToolsButtonIndex)):
            case menuButton: Button =>
              menuButton.isDisabled should be(expectedIsDisableValue)

  test("Performance Button is disabled exactly while the GameEngine is running"):
    val cases = Table(
      ("isEngineRunning", "expectedIsDisableValue"),
      (FreshSceneCannotBeUpdated, true),
      (FreshSceneCanBeUpdated, false)
    )

    forAll(cases): (isEngineRunning, expectedIsDisableValue) =>
      val builderResult = getOrFail(
        ModePanel.build(
          ImageConfigRecord,
          OnModeChange,
          OnStopClick,
          isEngineRunning
        )
      )

      inside(builderResult.children.head):
        case buttonsRow: HBox =>
          inside(buttonsRow.children.get(PerformanceButtonIndex)):
            case performanceButton: Button =>
              performanceButton.isDisabled shouldBe expectedIsDisableValue

  test("a disabled Performance Button does not run its configured action"):
    var performanceTestRuns = 0
    val configuredModePanel =
      GameEngineModePanel.withPerformanceExperiment(() => performanceTestRuns += 1)
    val panel = getOrFail(
      configuredModePanel.build(
        ImageConfigRecord,
        OnModeChange,
        OnStopClick,
        FreshSceneCannotBeUpdated
      )
    )

    onFxThread {
      val performanceButton = panel.delegate.getChildren.getFirst
        .asInstanceOf[HBox]
        .getChildren
        .get(PerformanceButtonIndex)
        .asInstanceOf[Button]

      performanceButton.fire()

      performanceTestRuns shouldBe 0
    }
