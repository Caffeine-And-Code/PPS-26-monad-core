package monad_core.simulator.presentation.performance

import monad_core.simulator.presentation.panels.GameEngineModePanel
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceModeTest extends AnyFunSuite with Matchers:

  test("panelFor returns the base panel by default"):
    val result = PerformanceMode.panelFor(Array.empty)

    result shouldBe GameEngineModePanel

  test("panelFor returns the performance panel when requested"):
    val result = PerformanceMode.panelFor(Array("--performance"))

    result shouldBe a[PerformanceGameEngineModePanel]
