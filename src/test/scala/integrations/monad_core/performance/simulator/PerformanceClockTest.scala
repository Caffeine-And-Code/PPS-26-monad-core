package integrations.monad_core.performance.simulator

import monad_core.performance.simulator.PerformanceClock
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceClockTest extends AnyFunSuite with Matchers:

  test("now returns a monotonic system reading"):
    val first = PerformanceClock.now()

    val second = PerformanceClock.now()

    second should be >= first
