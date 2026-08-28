package monad_core

import monad_core.simulator.presentation.panels.GameEngineModePanel
import monad_core.simulator.presentation.performance.PerformanceGameEngineModePanel
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LauncherTest extends AnyFunSuite with Inside with Matchers:

  test("the launcher selects the base mode panel without the performance option"):
    val result = Launcher.modePanelFor(Array.empty, () => ())

    result should be theSameInstanceAs GameEngineModePanel

  test("the launcher selects the performance decorator for the exact performance option"):
    var executions = 0

    val result = Launcher.modePanelFor(
      Array("ignored", Launcher.PerformanceGuiArgument),
      () => executions += 1
    )

    inside(result):
      case PerformanceGameEngineModePanel(delegate, onPerformanceExperiment) =>
        delegate should be theSameInstanceAs GameEngineModePanel
        onPerformanceExperiment()
        executions shouldBe 1

  test("the launcher ignores a similar but unsupported performance option"):
    val result = Launcher.modePanelFor(Array("performance"), () => ())

    result should be theSameInstanceAs GameEngineModePanel
