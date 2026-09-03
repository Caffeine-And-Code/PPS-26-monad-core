package monad_core.performance.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class PerformanceConfigTest extends AnyFunSuite with Matchers:

  private val Start      = 10
  private val Maximum    = 100
  private val Factor     = 2
  private val Iterations = 5
  private val Warmups    = 1
  private val Budget     = 16L

  private def config(
      start: Int = Start,
      maximum: Int = Maximum,
      factor: Int = Factor,
      iterations: Int = Iterations,
      warmups: Int = Warmups,
      budget: Long = Budget
  ): Either[PerformanceError, PerformanceConfig] =
    PerformanceConfig.from(start, maximum, factor, iterations, warmups, budget)

  test("from stores the entity growth"):
    val result = config().value

    val resultValue = result.growth.counts.value.map(_.value)

    resultValue shouldBe Vector(10, 20, 40, 80, 100)

  test("from stores the iteration count"):
    val result = config().value

    result.iterations.value shouldBe Iterations

  test("from stores the warm-up count"):
    val result = config().value

    result.warmups.value shouldBe Warmups

  test("from converts the frame budget from milliseconds"):
    val result = config().value

    result.frameBudget shouldBe Budget.millis

  test("from rejects an invalid entity growth"):
    val result = config(maximum = Start - 1)

    result shouldBe Left(InvalidGrowthMaximum(Start, Start - 1))

  test("from rejects an invalid starting entity count"):
    val result = config(start = 0)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("from rejects an invalid maximum entity count"):
    val result = config(maximum = 0)

    result shouldBe Left(InvalidPositiveCount("Entity count", 0))

  test("from rejects an invalid growth factor"):
    val result = config(factor = 1)

    result shouldBe Left(InvalidGrowthFactor(1))

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

  test("default uses the default start count"):
    val result = PerformanceConfig.default.value

    result.growth.start.value shouldBe PerformanceConfig.DefaultStartEntities

  test("default uses the default maximum count"):
    val result = PerformanceConfig.default.value

    result.growth.maximum.value shouldBe PerformanceConfig.DefaultMaximumEntities

  test("default uses the default growth factor"):
    val result = PerformanceConfig.default.value

    result.growth.factor.value shouldBe PerformanceConfig.DefaultGrowthFactor

  test("default uses the default iteration count"):
    val result = PerformanceConfig.default.value

    result.iterations.value shouldBe PerformanceConfig.DefaultIterations

  test("default uses the default warm-up count"):
    val result = PerformanceConfig.default.value

    result.warmups.value shouldBe PerformanceConfig.DefaultWarmups

  test("default uses the default frame budget"):
    val result = PerformanceConfig.default.value

    result.frameBudget shouldBe PerformanceConfig.DefaultFrameBudgetMillis.millis
