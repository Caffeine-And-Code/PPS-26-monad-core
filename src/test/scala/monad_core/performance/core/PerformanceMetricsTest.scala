package monad_core.performance.core

import monad_core.performance.model.EmptyPerformanceSamples
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class PerformanceMetricsTest extends AnyFunSuite with Matchers:

  private val OrderedSamples = (1L to 20L).map(PerformanceSample.apply).toVector

  test("latency rejects an empty sample collection"):
    val result = PerformanceMetrics.latency(Vector.empty)

    result shouldBe Left(EmptyPerformanceSamples())

  test("latency sorts samples before calculating percentiles"):
    val samples = Vector(3L, 1L, 2L).map(PerformanceSample.apply)

    val result = PerformanceMetrics.latency(samples).value

    result.p50Nanos shouldBe 2L

  test("latency calculates the nearest-rank median"):
    val result = PerformanceMetrics.latency(OrderedSamples).value

    result.p50Nanos shouldBe 10L

  test("latency calculates the nearest-rank ninety-fifth percentile"):
    val result = PerformanceMetrics.latency(OrderedSamples).value

    result.p95Nanos shouldBe 19L

  test("latency calculates the nearest-rank ninety-ninth percentile"):
    val result = PerformanceMetrics.latency(OrderedSamples).value

    result.p99Nanos shouldBe 20L

  test("latency uses the only sample as the median"):
    val result = PerformanceMetrics.latency(Vector(PerformanceSample(7L))).value

    result.p50Nanos shouldBe 7L

  test("latency uses the only sample as the ninety-fifth percentile"):
    val result = PerformanceMetrics.latency(Vector(PerformanceSample(7L))).value

    result.p95Nanos shouldBe 7L

  test("latency uses the only sample as the ninety-ninth percentile"):
    val result = PerformanceMetrics.latency(Vector(PerformanceSample(7L))).value

    result.p99Nanos shouldBe 7L

  test("completionRate rejects an empty sample collection"):
    val result = PerformanceMetrics.completionRate(Vector.empty, 1.millis)

    result shouldBe Left(EmptyPerformanceSamples())

  test("completionRate returns one when every sample meets the budget"):
    val samples = Vector(1L, 2L).map(PerformanceSample.apply)

    val result = PerformanceMetrics.completionRate(samples, 2.nanos).value

    result shouldBe 1.0

  test("completionRate returns zero when every sample exceeds the budget"):
    val samples = Vector(2L, 3L).map(PerformanceSample.apply)

    val result = PerformanceMetrics.completionRate(samples, 1.nano).value

    result shouldBe 0.0

  test("completionRate returns the fraction of samples meeting the budget"):
    val samples = Vector(1L, 2L, 3L, 4L).map(PerformanceSample.apply)

    val result = PerformanceMetrics.completionRate(samples, 2.nanos).value

    result shouldBe 0.5

  test("completionRate includes a sample equal to the budget"):
    val result = PerformanceMetrics
      .completionRate(Vector(PerformanceSample(2L)), 2.nanos)
      .value

    result shouldBe 1.0
