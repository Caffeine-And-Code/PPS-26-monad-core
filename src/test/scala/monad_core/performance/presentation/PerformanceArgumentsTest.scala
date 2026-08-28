package monad_core.performance.presentation

import monad_core.performance.domain.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceArgumentsTest extends AnyFunSuite with Matchers:

  test("performance arguments use a safe default configuration"):
    val result = PerformanceArguments.parse(Array.empty)

    result shouldBe PerformanceConfig.default

  test("performance arguments parse the requested entity count"):
    val requestedEntityCount = 250
    val args                 = Array(PerformanceArguments.Entities, requestedEntityCount.toString)

    val result = PerformanceArguments.parse(args)

    result.map(_.growth.start.value) shouldBe Right(requestedEntityCount)

  test("performance arguments use the first occurrence of a repeated option"):
    val firstEntityCount  = 250
    val secondEntityCount = 500
    val args = Array(
      PerformanceArguments.Entities,
      firstEntityCount.toString,
      PerformanceArguments.Entities,
      secondEntityCount.toString
    )

    val result = PerformanceArguments.parse(args)

    result.map(_.growth.start.value) shouldBe Right(firstEntityCount)

  test("performance arguments use the default when an option has no value"):
    val args = Array(PerformanceArguments.Entities)

    val result = PerformanceArguments.parse(args)

    result shouldBe PerformanceConfig.default

  test("performance arguments reject an invalid entity count"):
    val requestedEntityCount = 0
    val args                 = Array(PerformanceArguments.Entities, requestedEntityCount.toString)

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(InvalidEntityCount(requestedEntityCount))

  test("performance arguments reject an invalid parsed int argument"):
    val invalidArgument = "not-an-int"
    val args            = Array(PerformanceArguments.Entities, invalidArgument)

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(InvalidPerformanceArgument(PerformanceArguments.Entities, invalidArgument))

  test("performance arguments parse the requested maximum entity count"):
    val requestedMaximumEntityCount = 1000
    val args = Array(PerformanceArguments.MaximumEntities, requestedMaximumEntityCount.toString)

    val result = PerformanceArguments.parse(args)

    result.map(_.growth.maximum.value) shouldBe Right(requestedMaximumEntityCount)

  test("performance arguments reject an invalid couple of entity counts"):
    val requestedStart = 100
    val requestedMax   = 50
    val args = Array(
      PerformanceArguments.Entities,
      requestedStart.toString,
      PerformanceArguments.MaximumEntities,
      requestedMax.toString
    )

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(InvalidGrowthMaximum(requestedStart, requestedMax))

  test("performance arguments parse the requested growth factor"):
    val requestedGrowthFactor = 3
    val args = Array(PerformanceArguments.GrowthFactor, requestedGrowthFactor.toString)

    val result = PerformanceArguments.parse(args)

    result.map(_.growth.factor) shouldBe Right(requestedGrowthFactor)

  test("performance arguments reject an invalid growth factor"):
    val requestedGrowthFactor = 1
    val args = Array(PerformanceArguments.GrowthFactor, requestedGrowthFactor.toString)

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(InvalidGrowthFactor(requestedGrowthFactor))

  test("performance arguments parse the requested iteration number"):
    val requestedIterations = 10
    val args                = Array(PerformanceArguments.Iterations, requestedIterations.toString)

    val result = PerformanceArguments.parse(args)

    result.map(_.iterations.value) shouldBe Right(requestedIterations)

  test("performance arguments rejects an invalid iteration number"):
    val requestedIterations = 0
    val args                = Array(PerformanceArguments.Iterations, requestedIterations.toString)

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(InvalidIterationCount(requestedIterations))

  test("performance arguments parse the requested warmup number"):
    val requestedWarmups = 5
    val args             = Array(PerformanceArguments.Warmups, requestedWarmups.toString)

    val result = PerformanceArguments.parse(args)

    result.map(_.warmups.value) shouldBe Right(requestedWarmups)

  test("performance arguments reject an invalid warmup number"):
    val requestedWarmups = -1
    val args             = Array(PerformanceArguments.Warmups, requestedWarmups.toString)

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(InvalidWarmupCount(requestedWarmups))

  test("performance arguments parse the requested frame budget"):
    val requestedFrameBudgetMillis = 16
    val args = Array(PerformanceArguments.FrameBudgetMillis, requestedFrameBudgetMillis.toString)

    val result = PerformanceArguments.parse(args)

    result.map(_.frameBudget.nanos) shouldBe Right(requestedFrameBudgetMillis * 1_000_000L)

  test("performance arguments reject an invalid frame budget"):
    val requestedFrameBudgetMillis = 0
    val args = Array(PerformanceArguments.FrameBudgetMillis, requestedFrameBudgetMillis.toString)

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(InvalidFrameBudget(requestedFrameBudgetMillis))

  test("performance arguments reject an invalid parsed long argument"):
    val invalidArgument = "not-an-long"
    val args            = Array(PerformanceArguments.FrameBudgetMillis, invalidArgument)

    val result = PerformanceArguments.parse(args)

    result shouldBe Left(
      InvalidPerformanceArgument(PerformanceArguments.FrameBudgetMillis, invalidArgument)
    )

  test("performance arguments options should be defined correctly"):

    PerformanceArguments.Entities shouldBe "--entities"
    PerformanceArguments.MaximumEntities shouldBe "--max-entities"
    PerformanceArguments.GrowthFactor shouldBe "--growth-factor"
    PerformanceArguments.Iterations shouldBe "--iterations"
    PerformanceArguments.Warmups shouldBe "--warmups"
    PerformanceArguments.FrameBudgetMillis shouldBe "--frame-budget-ms"
