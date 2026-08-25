package monad_core.performance.domain

/**
 * Supported performance experiment strategies.
 *
 * Each strategy determines the sequence of workload sizes selected by the application layer.
 */
enum ExperimentKind:

  /** Measures sustained execution at the configured starting entity count. */
  case Load

  /** Increases the entity count until the p95 frame budget is exceeded or the maximum is reached. */
  case Stress

  /** Measures the transition from the starting count to the maximum and back to the start. */
  case Spike

  /** Measures every configured growth point to compare latency as the workload grows. */
  case Scalability

/**
 * Aggregate metrics collected for one entity count.
 *
 * @param entityCount
 *   workload size represented by this point
 * @param latency
 *   p50, p95, and p99 latency distribution of the collected samples
 * @param frameBudgetCompletionRate
 *   fraction of samples that completed within the configured frame budget, in `[0.0, 1.0]`
 */
final case class PerformancePoint(
    entityCount: EntityCount,
    latency: LatencyDistribution,
    frameBudgetCompletionRate: Double
)

/**
 * Result of a completed performance experiment.
 *
 * @param kind
 *   strategy used to run the experiment
 * @param points
 *   aggregate measurements in execution order
 * @param breakpoint
 *   first entity count whose p95 latency exceeded the frame budget; populated only by experiments
 *   configured to stop at that threshold
 */
final case class ExperimentReport(
    kind: ExperimentKind,
    points: Vector[PerformancePoint],
    breakpoint: Option[EntityCount]
)
