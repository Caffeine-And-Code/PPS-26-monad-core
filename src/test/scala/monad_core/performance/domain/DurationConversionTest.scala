package monad_core.performance.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DurationConversionTest extends AnyFunSuite with Matchers:

  private val Milliseconds         = 2L
  private val Nanoseconds          = 1_500_000L
  private val ExpectedNanoseconds  = 2_000_000L
  private val ExpectedMilliseconds = 1.5
  private val ExpectedWholeMillis  = 1L

  test("milliseconds are converted to nanoseconds"):
    val result = DurationConversion.millisToNanos(Milliseconds)

    result shouldBe ExpectedNanoseconds

  test("nanoseconds are converted to fractional milliseconds"):
    val result = DurationConversion.nanosToMillis(Nanoseconds)

    result shouldBe ExpectedMilliseconds

  test("nanoseconds are converted to whole milliseconds"):
    val result = DurationConversion.nanosToWholeMillis(Nanoseconds)

    result shouldBe ExpectedWholeMillis
