package monad_core.performance.application

import monad_core.performance.domain.*
import monad_core.performance.helpers.SequenceNanoClock
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExperimentRunnerTest extends AnyFunSuite with Matchers:

  private val StartingEntityCount = 100
  private val MaximumEntityCount  = 200
  private val GrowthFactor        = 2
  private val FrameBudgetNanos    = 16L
  private val ShortDurationNanos  = 1L

  private object SuccessfulWorkload extends PerformanceWorkload:

    override def prepare(
        entityCount: EntityCount
    ): Either[PerformanceError, SampleCollector.Operation] =
      Right(() => Right(()))

  private def boundaryConfig(
      maximumEntities: Int = MaximumEntityCount
  ): PerformanceConfig =
    PerformanceConfig
      .from(
        startEntities = StartingEntityCount,
        maximumEntities = maximumEntities,
        growthFactor = GrowthFactor,
        iterations = 1,
        warmups = 0,
        frameBudgetNanos = FrameBudgetNanos
      )
      .value

  test("a load experiment measures only the configured starting load"):
    val workload = SuccessfulWorkload
    val clock    = SequenceNanoClock(Vector(0L, 10L, 10L, 30L))
    val config = PerformanceConfig
      .from(
        startEntities = 100,
        maximumEntities = 400,
        growthFactor = 2,
        iterations = 2,
        warmups = 1,
        frameBudgetNanos = 16L
      )
      .value

    val result = ExperimentRunner.run(ExperimentKind.Load, config)(using workload, clock)

    val resultValue = result.map(_.points.map(_.entityCount.value))

    resultValue shouldBe Right(Vector(100))

  test("a load experiment does not report a stress breakpoint"):
    val workload = SuccessfulWorkload
    val clock    = SequenceNanoClock(Vector(0L, 20L))
    val config = PerformanceConfig
      .from(
        startEntities = 100,
        maximumEntities = 100,
        growthFactor = 2,
        iterations = 1,
        warmups = 1,
        frameBudgetNanos = 16L
      )
      .value

    val result = ExperimentRunner.run(ExperimentKind.Load, config)(using workload, clock)

    result.map(_.breakpoint) shouldBe Right(None)

  test("a stress experiment stops at the first load over the latency budget"):
    val workload = SuccessfulWorkload
    val clock = SequenceNanoClock(
      Vector(
        0L, 10L, 10L, 20L, 20L, 40L, 40L, 60L
      )
    )
    val config = PerformanceConfig
      .from(
        startEntities = 100,
        maximumEntities = 400,
        growthFactor = 2,
        iterations = 2,
        warmups = 1,
        frameBudgetNanos = 16L
      )
      .value

    val result = ExperimentRunner.run(ExperimentKind.Stress, config)(using workload, clock)

    val resultValue = result.map(_.breakpoint.map(_.value))

    resultValue shouldBe Right(Some(200))

  test("a stress experiment does not report a latency equal to the budget as a breakpoint"):
    val workload = SuccessfulWorkload
    val clock    = SequenceNanoClock(Vector(0L, FrameBudgetNanos))
    val config   = boundaryConfig(maximumEntities = StartingEntityCount)

    val result = ExperimentRunner.run(ExperimentKind.Stress, config)(using workload, clock)

    result.map(_.breakpoint) shouldBe Right(None)

  test("a stress experiment continues after a latency equal to the budget"):
    val workload = SuccessfulWorkload
    val clock = SequenceNanoClock(
      Vector(
        0L,
        FrameBudgetNanos,
        FrameBudgetNanos,
        FrameBudgetNanos + ShortDurationNanos
      )
    )
    val config = boundaryConfig()

    val result = ExperimentRunner.run(ExperimentKind.Stress, config)(using workload, clock)

    val resultValue = result.map(_.points.map(_.entityCount.value))

    resultValue shouldBe Right(Vector(StartingEntityCount, MaximumEntityCount))

  test("a spike experiment measures baseline, spike, then baseline again"):
    val workload = SuccessfulWorkload
    val clock = SequenceNanoClock(
      Vector(
        0L, 10L, 10L, 30L, 30L, 40L
      )
    )
    val config = PerformanceConfig
      .from(
        startEntities = 100,
        maximumEntities = 400,
        growthFactor = 2,
        iterations = 1,
        warmups = 1,
        frameBudgetNanos = 16L
      )
      .value

    val result = ExperimentRunner.run(ExperimentKind.Spike, config)(using workload, clock)

    val resultValue = result.map(_.points.map(_.entityCount.value))

    resultValue shouldBe Right(Vector(100, 400, 100))

  test("a scalability experiment should measure all configured loads"):
    val workload = SuccessfulWorkload
    val clock = SequenceNanoClock(
      Vector(
        0L, 10L, 10L, 20L, 20L, 30L, 30L, 40L
      )
    )
    val config = PerformanceConfig
      .from(
        startEntities = 100,
        maximumEntities = 400,
        growthFactor = 2,
        iterations = 1,
        warmups = 1,
        frameBudgetNanos = 16L
      )
      .value

    val result = ExperimentRunner.run(ExperimentKind.Scalability, config)(using workload, clock)

    val resultValue = result.map(_.points.map(_.entityCount.value))

    resultValue shouldBe Right(Vector(100, 200, 400))

  test("a scalability experiment continues after a latency over the budget"):
    val workload        = SuccessfulWorkload
    val overBudgetNanos = FrameBudgetNanos + ShortDurationNanos
    val finalTimestamp  = overBudgetNanos + ShortDurationNanos
    val clock = SequenceNanoClock(
      Vector(0L, overBudgetNanos, overBudgetNanos, finalTimestamp)
    )
    val config = boundaryConfig()

    val result = ExperimentRunner.run(ExperimentKind.Scalability, config)(using workload, clock)

    val resultValue = result.map(_.points.map(_.entityCount.value))

    resultValue shouldBe Right(Vector(StartingEntityCount, MaximumEntityCount))
