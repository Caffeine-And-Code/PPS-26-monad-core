package integrations.monad_core.simulator.presentation.performance

import helpers.mocks.{MockImage, MockImageConfig}
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.ScalaFxInit
import javafx.scene.control.Button
import javafx.scene.layout.HBox
import monad_core.simulator.{CannotBuildPanel, ImageResourceNotFound}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.infrastructure.engine.MonadCoreGameEngineRuntime
import monad_core.simulator.presentation.panels.GameEngineModePanel
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.performance.PerformanceGameEngineModePanel
import monad_core.simulator.presentation.resources.ImageConfigRecord
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

class PerformanceGameEngineModePanelTest
    extends AnyFunSuite
    with Inside
    with Matchers
    with MockFactory
    with ScalaFxInit:

  given world: World               = mock[World]
  given runtime: GameEngineRuntime = MonadCoreGameEngineRuntime()

  private val ImageConfig                   = MockImageConfig()
  private val PerformanceButtonIndex        = 2
  private val BaseControlCount              = 5
  private val OnModeChange: Boolean => Unit = _ => ()
  private val OnStopClick: () => Unit       = () => ()

  test("the performance decorator adds one control to the base mode panel"):
    val panel = getOrFail(
      PerformanceGameEngineModePanel(GameEngineModePanel, () => ()).build(
        ImageConfig,
        OnModeChange,
        OnStopClick,
        BooleanProperty(false)
      )
    )

    inside(panel.children.head):
      case buttonsRow: HBox =>
        buttonsRow.getChildren.size shouldBe BaseControlCount + 1
        buttonsRow.getChildren.get(PerformanceButtonIndex) shouldBe a[Button]

  test("the performance control executes its configured action"):
    var executions = 0
    val panel = getOrFail(
      PerformanceGameEngineModePanel(GameEngineModePanel, () => executions += 1).build(
        ImageConfig,
        OnModeChange,
        OnStopClick,
        BooleanProperty(false)
      )
    )

    onFxThread {
      performanceButtonOf(panel).fire()

      executions shouldBe 1
    }

  test("the performance control is disabled exactly while the engine is running"):
    val cases = Table(
      ("isEngineRunning", "expectedDisabled"),
      (false, false),
      (true, true)
    )

    forAll(cases): (isEngineRunning, expectedDisabled) =>
      val panel = getOrFail(
        PerformanceGameEngineModePanel(GameEngineModePanel, () => ()).build(
          ImageConfig,
          OnModeChange,
          OnStopClick,
          BooleanProperty(isEngineRunning)
        )
      )

      performanceButtonOf(panel).isDisabled shouldBe expectedDisabled

  test("a disabled performance control does not execute its configured action"):
    var executions = 0
    val panel = getOrFail(
      PerformanceGameEngineModePanel(GameEngineModePanel, () => executions += 1).build(
        ImageConfig,
        OnModeChange,
        OnStopClick,
        BooleanProperty(true)
      )
    )

    onFxThread {
      performanceButtonOf(panel).fire()

      executions shouldBe 0
    }

  test("the performance decorator preserves a base-panel failure"):
    val delegate = mock[GameEngineModePanelBuilder]
    val expected = CannotBuildPanel(ImageResourceNotFound(MockImage()), "base")
    (delegate
      .build(_: ImageConfigRecord, _: Boolean => Unit, _: () => Unit, _: BooleanProperty)(using
        _: World,
        _: GameEngineRuntime
      ))
      .expects(*, *, *, *, *, *)
      .returns(Left(expected))

    val result = PerformanceGameEngineModePanel(delegate, () => ()).build(
      ImageConfig,
      OnModeChange,
      OnStopClick,
      BooleanProperty(false)
    )

    result shouldBe Left(expected)

  test("the performance decorator reports an invalid performance icon"):
    val delegate           = mock[GameEngineModePanelBuilder]
    val invalidImageConfig = mock[ImageConfigRecord]
    val basePanel          = new VBox()
    basePanel.delegate.getChildren.add(new HBox())
    (delegate
      .build(_: ImageConfigRecord, _: Boolean => Unit, _: () => Unit, _: BooleanProperty)(using
        _: World,
        _: GameEngineRuntime
      ))
      .expects(invalidImageConfig, *, *, *, *, *)
      .returns(Right(basePanel))

    val result = PerformanceGameEngineModePanel(delegate, () => ()).build(
      invalidImageConfig,
      OnModeChange,
      OnStopClick,
      BooleanProperty(false)
    )

    inside(result):
      case Left(error) => error shouldBe a[CannotBuildPanel]

  private def performanceButtonOf(panel: VBox): Button =
    panel.delegate.getChildren.getFirst
      .asInstanceOf[HBox]
      .getChildren
      .get(PerformanceButtonIndex)
      .asInstanceOf[Button]
