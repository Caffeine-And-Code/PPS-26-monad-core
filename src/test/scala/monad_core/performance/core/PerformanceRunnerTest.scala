package monad_core.performance.core

import monad_core.performance.helpers.SequenceNanoClock
import monad_core.performance.model.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceRunnerTest extends AnyFunSuite with Matchers:

  private type Operation = () => Either[PerformanceError, Unit]
  private type Prepare   = EntityCount => Either[PerformanceError, Operation]

  private val StartEntities   = 2
  private val MaximumEntities = 8
  private val GrowthFactor    = 2
  private val OneMillisecond  = 1L
  private val WithinBudget    = 500_000L
  private val AtBudget        = 1_000_000L
  private val AboveBudget     = 2_000_000L

  private def config(
      start: Int = StartEntities,
      maximum: Int = MaximumEntities,
      iterations: Int = 1,
      warmups: Int = 0,
      budgetMillis: Long = OneMillisecond
  ): PerformanceConfig =
    PerformanceConfig
      .from(start, maximum, GrowthFactor, iterations, warmups, budgetMillis)
      .value

  private def request(
      kind: PerformanceKind,
      performanceConfig: PerformanceConfig = config()
  ): PerformanceRequest =
    PerformanceRequest(kind, performanceConfig)

  private def clockFor(durations: Vector[Long]): SequenceNanoClock =
    SequenceNanoClock(durations.flatMap(duration => Vector(0L, duration)))

  private def successfulPrepare(
      onExecution: () => Unit = () => ()
  ): Prepare = _ =>
    Right(() =>
      onExecution()
      Right(())
    )

  private def run(
      kind: PerformanceKind,
      durations: Vector[Long],
      performanceConfig: PerformanceConfig = config(),
      prepare: Prepare = successfulPrepare()
  ): Either[PerformanceError, PerformanceReport] =
    given NanoClock = clockFor(durations)
    PerformanceRunner.run(request(kind, performanceConfig), prepare)

  test("Load measures only the starting entity count"):
    val result = run(PerformanceKind.Load, Vector(WithinBudget)).value

    val resultValue = result.points.map(_.entityCount.value)

    resultValue shouldBe Vector(StartEntities)

  test("Spike measures the starting, maximum, and recovery entity counts"):
    val result = run(
      PerformanceKind.Spike,
      Vector(WithinBudget, WithinBudget, WithinBudget)
    ).value

    val resultValue = result.points.map(_.entityCount.value)

    resultValue shouldBe
      Vector(StartEntities, MaximumEntities, StartEntities)

  test("Scalability measures every generated entity count"):
    val result = run(
      PerformanceKind.Scalability,
      Vector(WithinBudget, WithinBudget, WithinBudget)
    ).value

    val resultValue = result.points.map(_.entityCount.value)

    resultValue shouldBe Vector(2, 4, 8)

  test("Stress stops after the first entity count exceeding the frame budget"):
    val result = run(
      PerformanceKind.Stress,
      Vector(WithinBudget, AboveBudget)
    ).value

    val resultValues = result.points.map(_.entityCount.value)

    resultValues shouldBe Vector(2, 4)

  test("Stress records the first entity count exceeding the frame budget"):
    val result = run(
      PerformanceKind.Stress,
      Vector(WithinBudget, AboveBudget)
    ).value

    val resultBreakpoint = result.breakpoint.value.value

    resultBreakpoint shouldBe 4

  test("Stress measures every count when no point exceeds the frame budget"):
    val result = run(
      PerformanceKind.Stress,
      Vector(WithinBudget, WithinBudget, WithinBudget)
    ).value

    val resultValues = result.points.map(_.entityCount.value)

    resultValues shouldBe Vector(2, 4, 8)

  test("Stress has no breakpoint when every point meets the frame budget"):
    val result = run(
      PerformanceKind.Stress,
      Vector(WithinBudget, WithinBudget, WithinBudget)
    ).value

    result.breakpoint shouldBe None

  test("a latency equal to the frame budget does not stop Stress"):
    val result = run(
      PerformanceKind.Stress,
      Vector(AtBudget, AtBudget, AtBudget)
    ).value

    val resultValue = result.points.map(_.entityCount.value)

    resultValue shouldBe Vector(2, 4, 8)

  test("Stress has no breakpoint when latency equals the frame budget"):
    val result = run(
      PerformanceKind.Stress,
      Vector(AtBudget, AtBudget, AtBudget)
    ).value

    result.breakpoint shouldBe None

  test("Load reports its selected experiment kind"):
    val result = run(PerformanceKind.Load, Vector(WithinBudget)).value

    result.kind shouldBe PerformanceKind.Load

  test("non-stress experiments do not report a breakpoint"):
    val result = run(PerformanceKind.Load, Vector(AboveBudget)).value

    result.breakpoint shouldBe None

  test("the runner executes every configured warm-up"):
    var executions = 0
    val configured = config(warmups = 2)

    run(
      PerformanceKind.Load,
      Vector(WithinBudget),
      configured,
      successfulPrepare(() => executions += 1)
    )

    executions shouldBe 3

  test("warm-ups do not consume clock readings"):
    val configured = config(warmups = 2)

    val result = run(
      PerformanceKind.Load,
      Vector(WithinBudget),
      configured
    )

    result shouldBe a[Right[?, ?]]

  test("the runner executes every configured measured iteration"):
    var executions = 0
    val configured = config(iterations = 3)

    run(
      PerformanceKind.Load,
      Vector(WithinBudget, WithinBudget, WithinBudget),
      configured,
      successfulPrepare(() => executions += 1)
    )

    executions shouldBe 3

  test("the runner reports the measured median"):
    val configured = config(iterations = 3)

    val result = run(
      PerformanceKind.Load,
      Vector(100L, 300L, 200L),
      configured
    ).value

    val resultValue = result.points.head.latency.p50Nanos

    resultValue shouldBe 200L

  test("the runner reports the measured ninety-fifth percentile"):
    val configured = config(iterations = 3)

    val result = run(
      PerformanceKind.Load,
      Vector(100L, 300L, 200L),
      configured
    ).value

    val resultValue = result.points.head.latency.p95Nanos

    resultValue shouldBe 300L

  test("the runner reports the measured ninety-ninth percentile"):
    val configured = config(iterations = 3)

    val result = run(
      PerformanceKind.Load,
      Vector(100L, 300L, 200L),
      configured
    ).value

    val resultValue = result.points.head.latency.p99Nanos

    resultValue shouldBe 300L

  test("the runner reports the frame-budget completion rate"):
    val configured = config(iterations = 3)

    val result = run(
      PerformanceKind.Load,
      Vector(WithinBudget, AtBudget, AboveBudget),
      configured
    ).value

    val resultValue = result.points.head.frameBudgetCompletionRate

    resultValue shouldBe (2.0 / 3.0)

  test("the runner propagates a workload preparation error"):
    val expected         = InvalidPerformanceArgument("prepare", "failed")
    val prepare: Prepare = _ => Left(expected)

    val result = run(PerformanceKind.Load, Vector(WithinBudget), prepare = prepare)

    result shouldBe Left(expected)

  test("the runner propagates a warm-up execution error"):
    val expected         = InvalidPerformanceArgument("warm-up", "failed")
    val configured       = config(warmups = 1)
    val prepare: Prepare = _ => Right(() => Left(expected))

    val result = run(PerformanceKind.Load, Vector.empty, configured, prepare)

    result shouldBe Left(expected)

  test("the runner propagates a measured execution error"):
    val expected         = InvalidPerformanceArgument("measurement", "failed")
    val prepare: Prepare = _ => Right(() => Left(expected))

    val result = run(PerformanceKind.Load, Vector(WithinBudget), prepare = prepare)

    result shouldBe Left(expected)
