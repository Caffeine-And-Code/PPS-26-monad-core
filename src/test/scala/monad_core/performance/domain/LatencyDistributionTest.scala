package monad_core.performance.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LatencyDistributionTest extends AnyFunSuite with Matchers:

  private val Samples = Vector(10L, 20L, 30L, 40L, 50L).map(PerformanceSample.apply)

  test("a latency distribution reports the median"):
    val result = LatencyDistribution.from(Samples)

    result.map(_.p50Nanos) shouldBe Right(30L)

  test("a latency distribution reports the ninety-fifth percentile"):
    val result = LatencyDistribution.from(Samples)

    result.map(_.p95Nanos) shouldBe Right(50L)

  test("a latency distribution reports the ninety-ninth percentile"):
    val result = LatencyDistribution.from(Samples)

    result.map(_.p99Nanos) shouldBe Right(50L)

  test("a latency distribution cannot be created without samples"):
    val result = LatencyDistribution.from(Vector.empty)

    result shouldBe Left(EmptyPerformanceSamples())
