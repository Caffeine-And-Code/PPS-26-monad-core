package monad_core.performance.model

/** Validated non-negative number of warm-up executions. */
opaque type WarmupCount = Int

/** Provides validated construction and operations for warm-up counts. */
object WarmupCount:

  /**
   * Creates a non-negative warm-up count.
   *
   * Zero is valid because an experiment may run without warm-up executions.
   *
   * @param value
   *   warm-up count to validate
   * @return
   *   the validated count, or an error when `value` is negative
   * @see
   *   [[monad_core.performance.model.InvalidWarmupCount InvalidWarmupCount]]
   */
  def from(value: Int): Either[PerformanceError, WarmupCount] =
    Either.cond(value >= 0, value, InvalidWarmupCount(value))

  extension (count: WarmupCount) def value: Int = count
