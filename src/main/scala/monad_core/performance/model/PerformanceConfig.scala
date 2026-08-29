package monad_core.performance.model

import scala.concurrent.duration.*

/**
 * Strategy used to vary the entity count during a performance experiment.
 *
 * Load measures the expected count, Stress searches for the frame-budget breakpoint, Spike
 * introduces a sudden increase and recovery, and Scalability measures the complete growth.
 */
enum PerformanceKind:
  case Load, Stress, Spike, Scalability

/**
 * Validated settings shared by every performance strategy.
 *
 * @param growth
 *   entity-count progression
 * @param iterations
 *   measured executions for each entity count
 * @param warmups
 *   unmeasured executions performed before collection
 * @param frameBudget
 *   maximum desired duration of one frame
 * @see
 *   [[scala.concurrent.duration.FiniteDuration FiniteDuration]]
 */
final case class PerformanceConfig private (
    growth: EntityGrowth,
    iterations: IterationCount,
    warmups: WarmupCount,
    frameBudget: FiniteDuration
)

/** Provides default values and validated construction for performance configurations. */
object PerformanceConfig:
  /** Default initial number of entities. */
  val DefaultStartEntities     = 100

  /** Default maximum number of entities. */
  val DefaultMaximumEntities   = 1_600

  /** Default multiplier between consecutive entity counts. */
  val DefaultGrowthFactor      = 2

  /** Default number of measured executions for each entity count. */
  val DefaultIterations        = 20

  /** Default number of unmeasured executions before collection. */
  val DefaultWarmups           = 5

  /** Default frame budget expressed in milliseconds. */
  val DefaultFrameBudgetMillis = 16L

  /**
   * Validates and creates a performance configuration.
   *
   * @param startEntities
   *   first entity count
   * @param maximumEntities
   *   greatest entity count
   * @param growthFactor
   *   multiplier applied between entity counts
   * @param iterations
   *   measured executions for each entity count
   * @param warmups
   *   unmeasured executions performed before collection
   * @param frameBudgetMillis
   *   positive frame budget in milliseconds
   * @return
   *   the validated configuration, or the first invalid argument
   * @see
   *   [[monad_core.performance.model.EntityGrowth EntityGrowth]],
   *   [[monad_core.performance.model.IterationCount IterationCount]] and
   *   [[monad_core.performance.model.WarmupCount WarmupCount]]
   */
  def from(
      startEntities: Int,
      maximumEntities: Int,
      growthFactor: Int,
      iterations: Int,
      warmups: Int,
      frameBudgetMillis: Long
  ): Either[PerformanceError, PerformanceConfig] =
    for
      growth         <- EntityGrowth.from(startEntities, maximumEntities, growthFactor)
      iterationCount <- IterationCount.from(iterations)
      warmupCount    <- WarmupCount.from(warmups)
      _ <- Either.cond(
        frameBudgetMillis > 0L,
        (),
        InvalidFrameBudget(frameBudgetMillis)
      )
    yield PerformanceConfig(
      growth,
      iterationCount,
      warmupCount,
      frameBudgetMillis.millis
    )

  /** Validated configuration built from all default values. */
  val default: Either[PerformanceError, PerformanceConfig] =
    from(
      DefaultStartEntities,
      DefaultMaximumEntities,
      DefaultGrowthFactor,
      DefaultIterations,
      DefaultWarmups,
      DefaultFrameBudgetMillis
    )
