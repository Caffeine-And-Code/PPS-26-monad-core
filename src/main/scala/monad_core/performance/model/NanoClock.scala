package monad_core.performance.model

/** Monotonic nanosecond clock used to make performance measurements replaceable and testable. */
trait NanoClock:
  /**
   * Reads the current monotonic timestamp.
   *
   * @return
   *   current timestamp in nanoseconds
   */
  def now(): Long
