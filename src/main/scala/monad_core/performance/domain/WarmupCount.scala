package monad_core.performance.domain

/** Validated number of unmeasured warm-up executions. */
opaque type WarmupCount = Int

/** Factory methods for [[WarmupCount]]. */
object WarmupCount:

  /**
   * Validates and creates a warm-up count.
   *
   * @param value
   *   candidate number of warm-up executions
   * @return
   *   the validated count when `value` is non-negative, otherwise [[InvalidWarmupCount]]
   */
  def from(value: Int): Either[PerformanceError, WarmupCount] =
    Either.cond(value >= 0, value, InvalidWarmupCount(value))

  extension (warmupCount: WarmupCount)
    /** Returns the validated primitive value. */
    def value: Int = warmupCount
