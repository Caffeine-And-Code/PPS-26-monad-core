package monad_core.performance.model

import scala.concurrent.duration.*

/**
 * Validated measurement settings shared by every performance strategy.
 *
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
    iterations: IterationCount,
    warmups: WarmupCount,
    frameBudget: FiniteDuration
)

/** Provides default values and validated construction for performance configurations. */
object PerformanceConfig:
  /** Default number of measured executions for each entity count. */
  val DefaultIterations = 20

  /** Default number of unmeasured executions before collection. */
  val DefaultWarmups = 5

  /** Default frame budget expressed in milliseconds. */
  val DefaultFrameBudgetMillis = 16L

  /**
   * Validates and creates a performance configuration.
   *
   * @param iterations
   *   measured executions for each entity count
   * @param warmups
   *   unmeasured executions performed before collection
   * @param frameBudgetMillis
   *   positive frame budget in milliseconds
   * @return
   *   the validated configuration, or the first invalid argument
   * @see
   *   [[monad_core.performance.model.IterationCount IterationCount]] and
   *   [[monad_core.performance.model.WarmupCount WarmupCount]]
   */
  def from(
      iterations: Int,
      warmups: Int,
      frameBudgetMillis: Long
  ): Either[PerformanceError, PerformanceConfig] =
    for
      iterationCount <- IterationCount.from(iterations)
      warmupCount    <- WarmupCount.from(warmups)
      _ <- Either.cond(
        frameBudgetMillis > 0L,
        (),
        InvalidFrameBudget(frameBudgetMillis)
      )
    yield PerformanceConfig(
      iterationCount,
      warmupCount,
      frameBudgetMillis.millis
    )

  /** Validated configuration built from all default values. */
  val default: Either[PerformanceError, PerformanceConfig] =
    from(
      DefaultIterations,
      DefaultWarmups,
      DefaultFrameBudgetMillis
    )
