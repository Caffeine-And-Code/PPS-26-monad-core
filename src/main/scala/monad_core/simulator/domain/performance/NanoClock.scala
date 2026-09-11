package monad_core.simulator.domain.performance

/** Monotonic nanosecond clock used to make performance measurements replaceable and testable. */
trait NanoClock:
  /**
   * Reads the current monotonic timestamp.
   *
   * @return
   *   current timestamp in nanoseconds
   */
  def now(): Long
