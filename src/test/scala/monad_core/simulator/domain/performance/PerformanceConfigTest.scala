package monad_core.simulator.domain.performance

import monad_core.simulator.domain.performance.{
  InvalidFrameBudget,
  InvalidPositiveCount,
  InvalidWarmupCount,
  PerformanceConfig,
  PerformanceError
}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class PerformanceConfigTest extends AnyFunSuite with Matchers:

  private val Iterations = 5
  private val Warmups    = 1
  private val Budget     = 16L

  private def config(
      iterations: Int = Iterations,
      warmups: Int = Warmups,
      budget: Long = Budget
  ): Either[PerformanceError, PerformanceConfig] =
    PerformanceConfig.from(iterations, warmups, budget)

  test("from stores the iteration count"):
    val result = config().value

    result.iterations.value shouldBe Iterations

  test("from stores the warm-up count"):
    val result = config().value

    result.warmups.value shouldBe Warmups

  test("from converts the frame budget from milliseconds"):
    val result = config().value

    result.frameBudget shouldBe Budget.millis

  test("from rejects an invalid iteration count"):
    val result = config(iterations = 0)

    result shouldBe Left(InvalidPositiveCount("Iteration count", 0))

  test("from rejects an invalid warm-up count"):
    val result = config(warmups = -1)

    result shouldBe Left(InvalidWarmupCount(-1))

  test("from rejects a zero frame budget"):
    val result = config(budget = 0L)

    result shouldBe Left(InvalidFrameBudget(0L))

  test("from rejects a negative frame budget"):
    val result = config(budget = -1L)

    result shouldBe Left(InvalidFrameBudget(-1L))

  test("default uses the default iteration count"):
    val result = PerformanceConfig.default.value

    result.iterations.value shouldBe 20

  test("default uses the default warm-up count"):
    val result = PerformanceConfig.default.value

    result.warmups.value shouldBe 5

  test("default uses the default frame budget"):
    val result = PerformanceConfig.default.value

    result.frameBudget shouldBe 16.millis
