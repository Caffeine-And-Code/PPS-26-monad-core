package monad_core.simulator.domain.performance

/**
 * Validated interval between the initial and maximum entity counts.
 *
 * @param start
 *   first entity count
 * @param maximum
 *   greatest entity count, never lower than `start`
 */
final case class EntityRange private (
    start: EntityCount,
    maximum: EntityCount
)

object EntityRange:

  /**
   * Creates an entity-count interval.
   *
   * @param start
   *   first entity count
   * @param maximum
   *   greatest entity count
   * @return
   *   the validated interval, or the first invalid argument
   * @see
   *   [[EntityCount EntityCount]]
   */
  def from(start: Int, maximum: Int): Either[PerformanceError, EntityRange] =
    for
      startCount   <- EntityCount.from(start)
      maximumCount <- EntityCount.from(maximum)
      _ <- Either.cond(
        maximumCount.value >= startCount.value,
        (),
        InvalidGrowthMaximum(start, maximum)
      )
    yield EntityRange(startCount, maximumCount)
