package integrations.monad_core.simulator.presentation.stages

import helpers.MockImage
import monad_core.simulator.presentation.stages.ScalaFxLauncher
import monad_core.simulator.presentation.stages.traits.MainStageBuilder
import monad_core.simulator.{CannotBuildStage, ImageResourceNotFound}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.beans.property.ReadOnlyDoubleProperty
import scalafx.scene.layout.HBox

class ScalaFxLauncherTest extends AnyFunSuite with Matchers with MockFactory:

  test("ScalaFxLauncher starts up, shows the stage, and shuts down cleanly"):
    val mainStage: MainStageBuilder = mock[MainStageBuilder]
    (mainStage.buildRootContent(_: ReadOnlyDoubleProperty, _: ReadOnlyDoubleProperty))
      .expects(*, *)
      .returns(Right(new HBox { children = Seq() }))

    val launcher = new ScalaFxLauncher(mainStage)

    val result = launcher.run()
    result shouldBe a[Right[?, ?]]