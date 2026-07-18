package integrations.monad_core.simulator.presentation.stages

import helpers.MockImage
import monad_core.simulator.presentation.stages.ScalaFxLauncher
import monad_core.simulator.presentation.stages.traits.MainStageBuilder
import monad_core.simulator.{CannotBuildStage, ImageResourceNotFound}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.beans.property.ReadOnlyDoubleProperty

class ScalaFxLauncherFailureTest extends AnyFunSuite with Matchers with MockFactory:

  test("run returns Left when buildRootContent fails, without showing anything"):
    val mainStage: MainStageBuilder = mock[MainStageBuilder]
    val expectedError = CannotBuildStage(ImageResourceNotFound(MockImage()), "")
    (mainStage.buildRootContent(_: ReadOnlyDoubleProperty, _: ReadOnlyDoubleProperty))
      .expects(*, *)
      .returns(Left(expectedError))

    val launcher = new ScalaFxLauncher(mainStage)
    launcher.run() shouldBe Left(expectedError)

