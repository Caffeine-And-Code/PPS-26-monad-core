package monad_core.performance.domain

/** Validated number of measured operation executions. */
opaque type IterationCount = Int

object IterationCount:

  /**
   * Validates and creates an iteration count.
   *
   * @param value
   *   candidate number of iterations
   * @return
   *   the validated count when `value` is strictly positive, otherwise [[InvalidIterationCount]]
   */
  def from(value: Int): Either[PerformanceError, IterationCount] =
    Either.cond(value > 0, value, InvalidIterationCount(value))

  extension (iterationCount: IterationCount)
    def value: Int = iterationCount
