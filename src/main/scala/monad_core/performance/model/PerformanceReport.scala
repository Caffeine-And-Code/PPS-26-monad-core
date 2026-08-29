package monad_core.performance.model

/**
 * Nearest-rank latency percentiles calculated from measured executions.
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

/**
 * Measurements collected for one entity count.
 *
 * @param entityCount
 *   number of entities used by the workload
 * @param latency
 *   measured latency distribution
 * @param frameBudgetCompletionRate
 *   fraction of measured executions completed within the frame budget
 */
final case class PerformancePoint(
    entityCount: EntityCount,
    latency: LatencyDistribution,
    frameBudgetCompletionRate: Double
)

/**
 * Complete result of one performance experiment.
 *
 * @param kind
 *   executed performance strategy
 * @param points
 *   measurements ordered by execution
 * @param breakpoint
 *   first entity count whose p95 exceeded the frame budget, when found by Stress
 */
final case class PerformanceReport(
    kind: PerformanceKind,
    points: Vector[PerformancePoint],
    breakpoint: Option[EntityCount]
)
