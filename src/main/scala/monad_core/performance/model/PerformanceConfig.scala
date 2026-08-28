package monad_core.performance.model

import scala.concurrent.duration.*

enum PerformanceKind:
  case Load, Stress, Spike, Scalability

final case class PerformanceConfig private (
    growth: EntityGrowth,
    iterations: IterationCount,
    warmups: WarmupCount,
    frameBudget: FiniteDuration
)

object PerformanceConfig:
  val DefaultStartEntities     = 100
  val DefaultMaximumEntities   = 1_600
  val DefaultGrowthFactor      = 2
  val DefaultIterations        = 20
  val DefaultWarmups           = 5
  val DefaultFrameBudgetMillis = 16L

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

  val default: Either[PerformanceError, PerformanceConfig] =
    from(
      DefaultStartEntities,
      DefaultMaximumEntities,
      DefaultGrowthFactor,
      DefaultIterations,
      DefaultWarmups,
      DefaultFrameBudgetMillis
    )
