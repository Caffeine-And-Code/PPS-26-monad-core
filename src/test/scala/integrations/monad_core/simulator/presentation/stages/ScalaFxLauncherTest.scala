package integrations.monad_core.simulator.presentation.stages

import helpers.MockImage
import monad_core.engine.errors.EngineError
import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.presentation.stages.ScalaFxLauncher
import monad_core.simulator.presentation.stages.traits.MainStageBuilder
import monad_core.simulator.{CannotBuildStage, ImageResourceNotFound, UnexpectedStartupFailure}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.scene.layout.HBox

import scala.concurrent.ExecutionContext

class ScalaFxLauncherTest extends AnyFunSuite with Matchers with MockFactory:
  given mockedAgent: AiAgent = mock[AiAgent]

  test("ScalaFxLauncher starts up, shows the stage, and shuts down cleanly"):
    val mainStage: MainStageBuilder = mock[MainStageBuilder]

    (mainStage.buildRootContent(_: ReadOnlyDoubleProperty, _: ReadOnlyDoubleProperty)(using _: AiAgent, _: ExecutionContext))
      .expects(*, *, *, *)
      .returns(Right(new HBox {
        children = Seq()
      }))

    val launcher = new ScalaFxLauncher(mainStage)

    val result = launcher.run()
    result shouldBe a[Right[?, ?]]

  test("run returns Left when buildRootContent fails, without showing anything"):
    val mainStage: MainStageBuilder = mock[MainStageBuilder]
    val expectedError = CannotBuildStage(ImageResourceNotFound(MockImage()), "")

    (mainStage.buildRootContent(_: ReadOnlyDoubleProperty, _: ReadOnlyDoubleProperty)(using _: AiAgent, _: ExecutionContext))
      .expects(*, *, *, *)
      .returns(Left(expectedError))

    val launcher = new ScalaFxLauncher(mainStage)
    launcher.run() shouldBe Left(expectedError)

  test("run returns Left(UnexpectedStartupFailure) when buildRootContent throws"):
    val mainStage: MainStageBuilder = mock[MainStageBuilder]
    val boom = new RuntimeException("boom")

    (mainStage.buildRootContent(_: ReadOnlyDoubleProperty, _: ReadOnlyDoubleProperty)(using _: AiAgent, _: ExecutionContext))
      .expects(*, *, *, *)
      .throws(boom)

    val launcher = new ScalaFxLauncher(mainStage)

    launcher.run() shouldBe Left(UnexpectedStartupFailure("boom"))