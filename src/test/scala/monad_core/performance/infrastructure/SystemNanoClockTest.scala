package monad_core.performance.infrastructure

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SystemNanoClockTest extends AnyFunSuite with Matchers:
  test("now should return the current system time in nanoseconds"):
    val before = System.nanoTime()

    val result = SystemNanoClock.now()

    val after = System.nanoTime()

    result should be > before
    result should be < after
