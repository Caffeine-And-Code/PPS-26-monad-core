package monad_core.performance.model

import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceReportTest extends AnyFunSuite with Matchers:

  private val EntityTotal = EntityCount.from(10).value
  private val Latency     = LatencyDistribution(1L, 2L, 3L)
  private val Point       = PerformancePoint(EntityTotal, Latency, 0.75)
  private val Report      = PerformanceReport(PerformanceKind.Load, Vector(Point), None)

  test("LatencyDistribution stores its median"):
    val result = Latency

    result.p50Nanos shouldBe 1L

  test("LatencyDistribution stores its ninety-fifth percentile"):
    val result = Latency

    result.p95Nanos shouldBe 2L

  test("LatencyDistribution stores its ninety-ninth percentile"):
    val result = Latency

    result.p99Nanos shouldBe 3L

  test("PerformancePoint stores its entity count"):
    val result = Point

    result.entityCount shouldBe EntityTotal

  test("PerformancePoint stores its latency distribution"):
    val result = Point

    result.latency shouldBe Latency

  test("PerformancePoint stores its frame-budget completion rate"):
    val result = Point

    result.frameBudgetCompletionRate shouldBe 0.75

  test("PerformanceReport stores its experiment kind"):
    val result = Report

    result.kind shouldBe PerformanceKind.Load

  test("PerformanceReport stores its measured points"):
    val result = Report

    result.points shouldBe Vector(Point)

  test("PerformanceReport stores its breakpoint"):
    val breakpoint = Some(EntityTotal)

    val result = PerformanceReport(PerformanceKind.Stress, Vector(Point), breakpoint)

    result.breakpoint shouldBe breakpoint
