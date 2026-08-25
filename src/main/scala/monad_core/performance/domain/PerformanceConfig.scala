package monad_core.performance.domain

/**
 * Validated settings shared by every performance experiment.
 *
 * @param growth
 *   starting count, maximum count, and growth factor for workload sizing
 * @param iterations
 *   number of measured executions per performance point
 * @param warmups
 *   number of unmeasured executions before each point
 * @param frameBudget
 *   target duration used for completion rates and stress breakpoints
 */
final case class PerformanceConfig(
    growth: EntityGrowth,
    iterations: IterationCount,
    warmups: WarmupCount,
    frameBudget: FrameBudget
)

/** Defaults and factory methods for [[PerformanceConfig]]. */
object PerformanceConfig:

  /** Default entity count at which experiments begin. */
  val DefaultStartEntities = 100

  /** Default inclusive upper entity count for growing experiments. */
  val DefaultMaximumEntities = 1_600

  /** Default multiplier applied between consecutive entity counts. */
  val DefaultGrowthFactor = 2

  /** Default number of measured executions per point. */
  val DefaultIterations = 20

  /** Default number of unmeasured warm-up executions per point. */
  val DefaultWarmups = 5

  /** Default frame budget in nanoseconds, equivalent to 16 milliseconds. */
  val DefaultFrameBudgetNanos = 16_000_000L

  /** Default configuration composed from all default values. */
  val default: Either[PerformanceError, PerformanceConfig] =
    from(
      startEntities = DefaultStartEntities,
      maximumEntities = DefaultMaximumEntities,
      growthFactor = DefaultGrowthFactor,
      iterations = DefaultIterations,
      warmups = DefaultWarmups,
      frameBudgetNanos = DefaultFrameBudgetNanos
    )

  /**
   * Validates primitive settings and creates a performance configuration.
   *
   * Validation delegates to the corresponding domain value objects and returns the first error in
   * growth, iteration, warm-up, then frame-budget order.
   *
   * @param startEntities
   *   positive starting entity count
   * @param maximumEntities
   *   positive maximum count not lower than `startEntities`
   * @param growthFactor
   *   multiplicative factor greater than one
   * @param iterations
   *   positive number of measured executions
   * @param warmups
   *   non-negative number of warm-up executions
   * @param frameBudgetNanos
   *   positive frame budget in nanoseconds
   * @return
   *   a validated configuration or the first domain validation error
   */
  def from(
      startEntities: Int,
      maximumEntities: Int,
      growthFactor: Int,
      iterations: Int,
      warmups: Int,
      frameBudgetNanos: Long
  ): Either[PerformanceError, PerformanceConfig] =
    for
      growth         <- EntityGrowth.from(startEntities, maximumEntities, growthFactor)
      iterationCount <- IterationCount.from(iterations)
      warmupCount    <- WarmupCount.from(warmups)
      frameBudget    <- FrameBudget.from(frameBudgetNanos)
    yield PerformanceConfig(growth, iterationCount, warmupCount, frameBudget)
