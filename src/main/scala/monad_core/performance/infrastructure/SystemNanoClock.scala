package monad_core.performance.infrastructure

import monad_core.performance.application.NanoClock

/**
 *  Production monotonic clock backed by `System.nanoTime()`.
 *
 * The returned value is suitable for elapsed-time measurement and is not a wall-clock timestamp.
 */
object SystemNanoClock extends NanoClock:

  /**
   * Reads the JVM's high-resolution monotonic time source.
   *
   * @return
   *   current monotonic clock value in nanoseconds
   */
  override def now(): Long = System.nanoTime()
