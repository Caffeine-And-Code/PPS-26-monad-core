package monad_core.performance.application

/**
 * Monotonic time source used to measure performance operations.
 *
 * Abstracting the clock keeps sample collection independent from the system clock and makes
 * elapsed-time measurements deterministic in tests. Implementations must return values expressed
 * in nanoseconds and suitable for computing durations by subtraction; the absolute origin has no
 * meaning.
 */
trait NanoClock:

  /**
   * Reads the current value of the monotonic clock.
   *
   * @return
   *   the current clock reading in nanoseconds
   */
  def now(): Long
