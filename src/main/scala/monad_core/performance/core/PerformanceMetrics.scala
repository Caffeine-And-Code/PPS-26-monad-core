package monad_core.performance.core

import monad_core.performance.model.{EmptyPerformanceSamples, LatencyDistribution, PerformanceError}

import scala.concurrent.duration.FiniteDuration

/**
 * Duration collected from one measured workload execution.
 *
 * @param durationNanos
 *   elapsed execution time in nanoseconds
 */
private[performance] final case class PerformanceSample(durationNanos: Long)

/**
 * Calculates latency percentiles and frame-budget completion rates.
 *
 * Latency uses the nearest-rank method: p50 is the median, while p95 and p99 are the
 * durations below which respectively 95% and 99% of the collected samples fall.
 */
private[performance] object PerformanceMetrics:
  private val Median      = 0.50
  private val NinetyFifth = 0.95
  private val NinetyNinth = 0.99

  /**
   * Calculates the p50, p95 and p99 latency distribution of the supplied samples.
   *
   * Input order does not affect the result because durations are sorted before calculating
   * each percentile.
   *
   * @param samples
   *   measured workload durations
   * @return
   *   the latency distribution, or an error when `samples` is empty
   * @see
   *   [[monad_core.performance.model.LatencyDistribution LatencyDistribution]]
   */
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

  /**
   * Calculates the fraction of samples completed within the supplied frame budget.
   *
   * A sample whose duration equals the budget is considered completed within it.
   *
   * @param samples
   *   measured workload durations
   * @param budget
   *   maximum duration accepted for one frame
   * @return
   *   a value between zero and one, or an error when `samples` is empty
   * @see
   *   [[scala.concurrent.duration.FiniteDuration FiniteDuration]]
   */
  def completionRate(
      samples: Vector[PerformanceSample],
      budget: FiniteDuration
  ): Either[PerformanceError, Double] =
    nonEmpty(samples).map(values =>
      values.count(_.durationNanos <= budget.toNanos).toDouble / values.size
    )

  /**
   * Calculates the nearest-rank percentile of a sorted collection of durations.
   *
   * @param sorted
   *   a vector of durations, sorted in ascending order
   * @param rank
   *   the percentile to calculate (e.g., 0.50 for p50)
   * @return
   *   the duration at the specified percentile
   */
  private def percentile(sorted: Vector[Long], rank: Double): Long =
    sorted(math.ceil(rank * sorted.size).toInt - 1)
  /**
   * Validates a vector of samples is not empty.
   *
   * @param samples
   *   a vector of performance samples
   * @return
   *   the same vector if not empty, or an error if empty
   * @see
   *   [[monad_core.performance.model.EmptyPerformanceSamples EmptyPerformanceSamples]]
   */
  private def nonEmpty(
      samples: Vector[PerformanceSample]
  ): Either[PerformanceError, Vector[PerformanceSample]] =
    Either.cond(samples.nonEmpty, samples, EmptyPerformanceSamples())
