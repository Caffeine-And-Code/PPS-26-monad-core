package monad_core.simulator.infrastructure.performance

import monad_core.simulator.domain.performance.NanoClock

/** Production monotonic clock used by performance measurements. */
object PerformanceClock extends NanoClock:
  /**
   * Reads the JVM monotonic clock.
   *
   * @return
   *   current monotonic timestamp in nanoseconds
   * @see
   *   [[java.lang.System System]]
   */
  override def now(): Long = System.nanoTime()
