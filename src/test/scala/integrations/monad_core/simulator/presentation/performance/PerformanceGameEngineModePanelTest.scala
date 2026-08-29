package integrations.monad_core.simulator.presentation.performance

import helpers.mocks.{MockImage, MockImageConfig}
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import javafx.scene.control.{Button, TextArea}
import javafx.scene.layout.HBox
import javafx.stage.{Stage, Window}
import monad_core.performance.simulator.PerformanceCli
import monad_core.simulator.{CannotBuildPanel, ImageResourceNotFound}
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.MonadCoreGameEngineRuntime
import monad_core.simulator.presentation.panels.GameEngineModePanel
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.performance.{
  ExperimentDialog,
  PerformanceGameEngineModePanel
}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import scalafx.beans.property.BooleanProperty
import scalafx.scene.layout.VBox

import scala.jdk.CollectionConverters.*

class PerformanceGameEngineModePanelTest
    extends AnyFunSuite
    with Matchers
    with Inside
    with MockFactory
    with DialogTesting
    with FormTesting
    with Eventually:

  given world: World                         = mock[World]
  given gameEngineRuntime: GameEngineRuntime = MonadCoreGameEngineRuntime()

  private val ImageConfig                   = MockImageConfig()
  private val PerformanceButtonIndex        = 2
  private val BaseControlCount              = 5
  private val OnModeChange: Boolean => Unit = _ => ()
  private val OnStopClick: () => Unit       = () => ()
  private val AsyncTimeout                  = Span(5, Seconds)
  private val AsyncPollingInterval          = Span(10, Millis)

  private def buildPanel(
      isEngineRunning: Boolean = false
  ): Either[BaseError, VBox] =
    PerformanceGameEngineModePanel(GameEngineModePanel).build(
      ImageConfig,
      OnModeChange,
      OnStopClick,
      BooleanProperty(isEngineRunning)
    )

  private def performanceButtonOf(panel: VBox): Button =
    panel.delegate.getChildren.getFirst
      .asInstanceOf[HBox]
      .getChildren
      .get(PerformanceButtonIndex)
      .asInstanceOf[Button]

  private def resultOutput: Option[TextArea] =
    Window.getWindows.asScala.collectFirst {
      case stage: Stage if stage.isShowing && stage.getTitle == ExperimentDialog.ResultTitle =>
        stage.getScene.getRoot
          .lookup(".performance-result-output")
          .asInstanceOf[TextArea]
    }

  test("build adds one control to the base panel"):
    val panel = getOrFail(buildPanel())

    val result = panel.delegate.getChildren.getFirst
      .asInstanceOf[HBox]
      .getChildren
      .size

    result shouldBe BaseControlCount + 1

  test("the performance button opens the experiment form"):
    val panel = getOrFail(buildPanel())

    onFxThread {
      performanceButtonOf(panel).fire()
    }

    onFxThread(getRequiredActiveStage.getTitle) shouldBe ExperimentDialog.Title

  test("the performance form executes its self-managed runner"):
    val panel = getOrFail(buildPanel())
    onFxThread {
      performanceButtonOf(panel).fire()
      allFormFields
        .find(_.getId == PerformanceCli.Entities)
        .value
        .setText("invalid")
      formSaveButton.fire()
    }

    eventually(timeout(AsyncTimeout), interval(AsyncPollingInterval)) {
      onFxThread(resultOutput.value.getText) should include("Invalid value")
    }

  test("the performance button is enabled while the engine is stopped"):
    val panel = getOrFail(buildPanel())

    val result = performanceButtonOf(panel).isDisabled

    result shouldBe false

  test("the performance button is disabled while the engine is running"):
    val panel = getOrFail(buildPanel(isEngineRunning = true))

    val result = performanceButtonOf(panel).isDisabled

    result shouldBe true

  test("a disabled performance button does not open the experiment form"):
    val panel = getOrFail(buildPanel(isEngineRunning = true))

    onFxThread {
      performanceButtonOf(panel).fire()
    }

    onFxThread(getActiveStage) shouldBe None

  test("build preserves a base-panel failure"):
    val delegate = mock[GameEngineModePanelBuilder]
    val expected = CannotBuildPanel(ImageResourceNotFound(MockImage()), "base")
    (delegate
      .build(_: ImageConfigRecord, _: Boolean => Unit, _: () => Unit, _: BooleanProperty)(using
        _: World,
        _: GameEngineRuntime
      ))
      .expects(*, *, *, *, *, *)
      .returns(Left(expected))

    val result = PerformanceGameEngineModePanel(delegate).build(
      ImageConfig,
      OnModeChange,
      OnStopClick,
      BooleanProperty(false)
    )

    result shouldBe Left(expected)

  test("build translates a performance-icon failure"):
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

    val result = PerformanceGameEngineModePanel(delegate).build(
      invalidImageConfig,
      OnModeChange,
      OnStopClick,
      BooleanProperty(false)
    )

    inside(result):
      case Left(error) => error shouldBe a[CannotBuildPanel]
