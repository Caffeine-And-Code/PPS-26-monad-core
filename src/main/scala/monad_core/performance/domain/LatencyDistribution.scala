package monad_core.performance.domain

/**
 * Nearest-rank latency percentiles for a collection of performance samples.
 *
 * @param p50Nanos
 *   median latency in nanoseconds
 * @param p95Nanos
 *   95th-percentile latency in nanoseconds
 * @param p99Nanos
 *   99th-percentile latency in nanoseconds
 */
final case class LatencyDistribution(
    p50Nanos: Long,
    p95Nanos: Long,
    p99Nanos: Long
)

/** Calculates [[LatencyDistribution]] values from raw samples. */
object LatencyDistribution:

  /** Quantile used for median latency. */
  private val MedianPercentile = 0.50

  /** Quantile used for p95 latency. */
  private val NinetyFifthPercentile = 0.95

  /** Quantile used for p99 latency. */
  private val NinetyNinthPercentile = 0.99

  /**
   * Calculates latency percentiles for a non-empty sample collection.
   *
   * Durations are sorted in ascending order before the nearest-rank method is applied.
   *
   * @param samples
   *   performance samples to aggregate
   * @return
   *   their p50, p95, and p99 distribution, or [[EmptyPerformanceSamples]] for an empty vector
   */
  def from(samples: Vector[PerformanceSample]): Either[PerformanceError, LatencyDistribution] =
    Either.cond(
      samples.nonEmpty,
      distribution(samples.map(_.durationNanos).sorted),
      EmptyPerformanceSamples()
    )

  /**
   * Builds all supported percentiles from sorted durations.
   *
   * @param sortedDurations
   *   non-empty durations sorted in ascending order
   * @return
   *   the resulting latency distribution
   */
  private def distribution(sortedDurations: Vector[Long]): LatencyDistribution =
    LatencyDistribution(
      p50Nanos = percentile(sortedDurations, MedianPercentile),
      p95Nanos = percentile(sortedDurations, NinetyFifthPercentile),
      p99Nanos = percentile(sortedDurations, NinetyNinthPercentile)
    )

  /**
   * Selects a percentile using the nearest-rank definition.
   *
   * @param sortedDurations
   *   non-empty durations sorted in ascending order
   * @param percentile
   *   quantile in the range `(0.0, 1.0]`
   * @return
   *   duration at rank `ceil(percentile * sampleCount)`
   */
  private def percentile(sortedDurations: Vector[Long], percentile: Double): Long =
    val nearestRank = math.ceil(percentile * sortedDurations.size).toInt
    sortedDurations(nearestRank - 1)
