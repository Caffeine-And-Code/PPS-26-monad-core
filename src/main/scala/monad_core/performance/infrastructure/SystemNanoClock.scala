package monad_core.performance.infrastructure

import monad_core.performance.application.NanoClock

/**
 * Produces a monotonic clock backed by `System.nanoTime()`.
 *
 * The returned value is suitable for elapsed-time measurements.
 */
object SystemNanoClock extends NanoClock:

  /**
   * Reads the JVM's high-resolution monotonic time source.
   *
   * @return
   *   current monotonic clock value in nanoseconds
   */
  override def now(): Long = System.nanoTime()
