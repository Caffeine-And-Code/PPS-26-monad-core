package monad_core.performance.domain

/** Pure conversions between the duration units used by performance experiments. */
object DurationConversion:

  /** Number of nanoseconds contained in one millisecond. */
  private val NanosecondsPerMillisecond = 1_000_000L

  /**
   * Converts an integral millisecond duration to nanoseconds.
   *
   * @param ms
   *   duration expressed in milliseconds
   * @return
   *   the same duration expressed in nanoseconds
   */
  def millisToNanos(ms: Long): Long =
    ms * NanosecondsPerMillisecond

  /**
   * Converts a nanosecond duration to fractional milliseconds.
   *
   * @param ns
   *   duration expressed in nanoseconds
   * @return
   *   the same duration expressed in milliseconds, preserving its fractional part
   */
  def nanosToMillis(ns: Long): Double =
    ns.toDouble / NanosecondsPerMillisecond

  /**
   * Converts a nanosecond duration to integral milliseconds.
   *
   * Any fractional millisecond is discarded through integer division.
   *
   * @param ns
   *   duration expressed in nanoseconds
   * @return
   *   the whole milliseconds contained in the supplied duration
   */
  def nanosToWholeMillis(ns: Long): Long =
    nanosToMillis(ns).toLong
