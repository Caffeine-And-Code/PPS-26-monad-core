package monad_core.performance.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NanoClockTest extends AnyFunSuite with Matchers:

  private val ExpectedReading = 42L

  private val clock = new NanoClock:
    override def now(): Long = ExpectedReading

  test("NanoClock provides its current nanosecond reading"):
    val result = clock.now()

    result shouldBe ExpectedReading
