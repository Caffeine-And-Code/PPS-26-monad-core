package integrations.monad_core.simulator.infrastructure.performance

import monad_core.simulator.infrastructure.performance.PerformanceClock
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceClockTest extends AnyFunSuite with Matchers:

  test("now returns a monotonic system reading"):
    val first = PerformanceClock.now()

    val second = PerformanceClock.now()

    second should be >= first
