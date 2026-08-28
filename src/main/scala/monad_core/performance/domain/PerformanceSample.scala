package monad_core.performance.domain

/**
 * Duration observed for one successful workload execution.
 *
 * @param durationNanos
 *   elapsed execution time in nanoseconds
 */
final case class PerformanceSample(durationNanos: Long)
