package integrations.monad_core.simulator

import helpers.MockImage
import integrations.monad_core.simulator.presentation.support.{ScalaFxInit, SnapshotTesting}
import monad_core.Launcher
import monad_core.simulator.{CannotBuildStage, ImageResourceNotFound}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.jfxStage2sfx

class LauncherTest extends AnyFunSuite with Matchers with SnapshotTesting with ScalaFxInit:

  test("outcomeFor returns a success outcome and message when the launcher succeeds"):
    val (success, message) = Launcher.outcomeFor(Right(()))

    success shouldBe true
    message should include("Build Completed")

  test("outcomeFor returns a failure outcome and includes the error message when the launcher fails"):
    val error = CannotBuildStage(ImageResourceNotFound(MockImage()), "")

    val (success, message) = Launcher.outcomeFor(Left(error))

    success shouldBe false
    message should include("Startup failed")
    message should include(error.message)

  test("buildLauncher generates a valid snapshot"):
    Launcher.main(Array.empty)

    val mainWindow = this.tryGetMainWindow

    mainWindow shouldBe defined

    assertMatchesSnapshotOfStage("launcher_scene_snapshot", mainWindow.get)