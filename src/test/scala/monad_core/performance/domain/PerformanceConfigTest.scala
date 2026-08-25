package monad_core.performance.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceConfigTest extends AnyFunSuite with Matchers:

  private val ValidStartEntities       = 100
  private val ValidMaximumEntities     = 400
  private val ValidGrowthFactor        = 2
  private val ValidIterations          = 10
  private val ValidWarmups             = 2
  private val ValidFrameBudgetNanos    = 16_000_000L
  private val InvalidEntityCountValue  = 0
  private val InvalidGrowthFactorValue = 1
  private val InvalidIterationValue    = 0
  private val InvalidWarmupValue       = -1
  private val InvalidFrameBudgetValue  = 0L

  private def configFrom(
      startEntities: Int = ValidStartEntities,
      maximumEntities: Int = ValidMaximumEntities,
      growthFactor: Int = ValidGrowthFactor,
      iterations: Int = ValidIterations,
      warmups: Int = ValidWarmups,
      frameBudgetNanos: Long = ValidFrameBudgetNanos
  ): Either[PerformanceError, PerformanceConfig] =
    PerformanceConfig.from(
      startEntities,
      maximumEntities,
      growthFactor,
      iterations,
      warmups,
      frameBudgetNanos
    )

  private def configValues(config: PerformanceConfig): (Int, Int, Int, Int, Int, Long) =
    (
      config.growth.start.value,
      config.growth.maximum.value,
      config.growth.factor,
      config.iterations.value,
      config.warmups.value,
      config.frameBudget.nanos
    )

  test("a performance configuration can be created from valid values"):
    val expectedValues = (
      ValidStartEntities,
      ValidMaximumEntities,
      ValidGrowthFactor,
      ValidIterations,
      ValidWarmups,
      ValidFrameBudgetNanos
    )

    val result = configFrom()

    result.map(configValues) shouldBe Right(expectedValues)

  test("the default performance configuration contains every default value"):
    val expectedValues = (
      PerformanceConfig.DefaultStartEntities,
      PerformanceConfig.DefaultMaximumEntities,
      PerformanceConfig.DefaultGrowthFactor,
      PerformanceConfig.DefaultIterations,
      PerformanceConfig.DefaultWarmups,
      PerformanceConfig.DefaultFrameBudgetNanos
    )

    val result = PerformanceConfig.default

    result.map(configValues) shouldBe Right(expectedValues)

  test("a performance configuration rejects an invalid starting entity count"):
    val invalidStart = InvalidEntityCountValue

    val result = configFrom(startEntities = invalidStart)

    result shouldBe Left(InvalidEntityCount(invalidStart))

  test("a performance configuration rejects an invalid maximum entity count"):
    val invalidMaximum = InvalidEntityCountValue

    val result = configFrom(maximumEntities = invalidMaximum)

    result shouldBe Left(InvalidEntityCount(invalidMaximum))

  test("a performance configuration rejects a maximum lower than its start"):
    val invalidMaximum = ValidStartEntities - 1

    val result = configFrom(maximumEntities = invalidMaximum)

    result shouldBe Left(InvalidGrowthMaximum(ValidStartEntities, invalidMaximum))

  test("a performance configuration rejects an invalid growth factor"):
    val invalidFactor = InvalidGrowthFactorValue

    val result = configFrom(growthFactor = invalidFactor)

    result shouldBe Left(InvalidGrowthFactor(invalidFactor))

  test("a performance configuration rejects an invalid iteration count"):
    val invalidIterations = InvalidIterationValue

    val result = configFrom(iterations = invalidIterations)

    result shouldBe Left(InvalidIterationCount(invalidIterations))

  test("a performance configuration rejects an invalid warm-up count"):
    val invalidWarmups = InvalidWarmupValue

    val result = configFrom(warmups = invalidWarmups)

    result shouldBe Left(InvalidWarmupCount(invalidWarmups))

  test("a performance configuration rejects an invalid frame budget"):
    val invalidFrameBudget = InvalidFrameBudgetValue

    val result = configFrom(frameBudgetNanos = invalidFrameBudget)

    result shouldBe Left(InvalidFrameBudget(invalidFrameBudget))

  test("a performance configuration returns the first validation error"):
    val invalidStart = InvalidEntityCountValue

    val result = configFrom(
      startEntities = invalidStart,
      iterations = InvalidIterationValue,
      warmups = InvalidWarmupValue,
      frameBudgetNanos = InvalidFrameBudgetValue
    )

    result shouldBe Left(InvalidEntityCount(invalidStart))
