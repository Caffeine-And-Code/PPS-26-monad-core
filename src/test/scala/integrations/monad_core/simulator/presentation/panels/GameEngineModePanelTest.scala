package integrations.monad_core.simulator.presentation.panels

import helpers.mocks.MockImageConfig
import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import javafx.scene.control.Button
import javafx.scene.layout.HBox
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.world.World
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

class GameEngineModePanelTest extends AnyFunSuite with Inside with Matchers with MockFactory with ScalaFxInit:
  given mockWorld: World = mock[World]

  val ToolsButtonIndex = 0
  val SpacingRegionIndex = 1
  val ModeButtonIndex = 2
  val StopButtonIndex = 3

  def freshSceneCanBeUpdated: BooleanProperty = BooleanProperty(false)

  def freshSceneCannotBeUpdated: BooleanProperty = BooleanProperty(true)

  test("A GameEngineModePanel can be created"):
    val imageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]

    val builderResult = GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated)

    inside(builderResult):
      case Right(scene) =>
        scene.children.getFirst shouldBe a[HBox]

  test("A GameEngineModePanel cannot be built when an invalid image config record is passed"):
    val imageConfigRecord: ImageConfigRecord = mock[ImageConfigRecord]
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]

    val builderResult = GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated)

    inside(builderResult):
      case Left(error) =>
        error shouldBe a[CannotBuildPanel]

  test("Mode Button Click event calls the passed onModeChange function with isActive equal to true after one click"):
    val imageConfigRecord: ImageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]

    val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated))

    onModeChange.expects(true).once()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn)


  test("Mode Button Click event calls the passed onModeChange function with isActive equal to false after two clicks"):
    val imageConfigRecord: ImageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]

    val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated))

    inSequence:
      onModeChange.expects(true).once()
      onModeChange.expects(false).once()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn, times = 2)

  test("Stop Button Click cannot be clicked upon scene start up when the isEngineRunning is false"):
    val imageConfigRecord: ImageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]
    val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated))

    onStopClick.expects().never()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click can be clicked upon scene start up when the isEngineRunning is true"):
    val imageConfigRecord: ImageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]
    val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCannotBeUpdated))

    onStopClick.expects().once()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click can be clicked once the PlayButton is clicked once"):
    val imageConfigRecord: ImageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]
    val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated))

    onModeChange.expects(true)
    onStopClick.expects()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn)
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click cannot be clicked once the PlayButton is clicked two times"):
    val imageConfigRecord: ImageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]
    val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated))

    inSequence:
      onModeChange.expects(true)
      onModeChange.expects(false)

    onStopClick.expects().never()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn, times = 2)
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Stop Button Click can be clicked when the PlayButton is active"):
    val imageConfigRecord: ImageConfigRecord = MockImageConfig()
    val onModeChange = mockFunction[Boolean, Unit]
    val onStopClick = mockFunction[Unit]
    val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, freshSceneCanBeUpdated))

    inSequence:
      onModeChange.expects(true)
      onModeChange.expects(false)
      onModeChange.expects(true)

    onStopClick.expects()

    inside(builderResult.children.head):
      case buttonsRow: HBox =>
        inside(buttonsRow.children.get(ModeButtonIndex)):
          case playPauseBtn: Button =>
            clickButton(playPauseBtn, times = 3)
        inside(buttonsRow.children.get(StopButtonIndex)):
          case stopButton: Button =>
            clickButton(stopButton)

  test("Tools Button cannot be clicked once the GameEngine is running and can be clicked when the engine is not running"):
    val cases = Table(
      ("isEngineRunning", "expectedIsDisableValue"),
      (freshSceneCannotBeUpdated, true),
      (freshSceneCanBeUpdated, false)
    )

    forAll(cases): (isEngineRunning, expectedIsDisableValue) =>
      val imageConfigRecord: ImageConfigRecord = MockImageConfig()
      val onModeChange = mockFunction[Boolean, Unit]
      val onStopClick = mockFunction[Unit]
      val builderResult = getOrFail(GameEngineModePanel.build(imageConfigRecord, onModeChange, onStopClick, isEngineRunning))

      onModeChange.expects(*).never()
      onStopClick.expects().never()

      inside(builderResult.children.head):
        case buttonsRow: HBox =>
          inside(buttonsRow.children.get(ToolsButtonIndex)):
            case menuButton: Button =>
              menuButton.isDisabled should be(expectedIsDisableValue)