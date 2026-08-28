package monad_core.performance.core

import monad_core.performance.model.{EmptyPerformanceSamples, LatencyDistribution, PerformanceError}

import scala.concurrent.duration.FiniteDuration

final case class PerformanceSample(durationNanos: Long)

/** Pure statistical calculations over performance samples. */
object PerformanceMetrics:
  private val Median      = 0.50
  private val NinetyFifth = 0.95
  private val NinetyNinth = 0.99

  def latency(
      samples: Vector[PerformanceSample]
  ): Either[PerformanceError, LatencyDistribution] = {
    nonEmpty(samples).map { values =>
      val sorted = values.map(_.durationNanos).sorted
      LatencyDistribution(
        percentile(sorted, Median),
        percentile(sorted, NinetyFifth),
        percentile(sorted, NinetyNinth)
      )
    }
  }

  def completionRate(
      samples: Vector[PerformanceSample],
      budget: FiniteDuration
  ): Either[PerformanceError, Double] =
    nonEmpty(samples).map(values =>
      values.count(_.durationNanos <= budget.toNanos).toDouble / values.size
    )

  private def percentile(sorted: Vector[Long], rank: Double): Long =
    sorted(math.ceil(rank * sorted.size).toInt - 1)

  private def nonEmpty(
      samples: Vector[PerformanceSample]
  ): Either[PerformanceError, Vector[PerformanceSample]] =
    Either.cond(samples.nonEmpty, samples, EmptyPerformanceSamples())
