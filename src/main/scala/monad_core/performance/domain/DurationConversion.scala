package monad_core.performance.domain

/** Pure conversions between the duration units used by performance experiments. */
object DurationConversion:

  private val NanosecondsPerMillisecond = 1_000_000L

  /**
   * Converts a duration in milliseconds to nanoseconds.
   *
   * @param ms
   *   duration in milliseconds
   * @return
   *   the same duration expressed in nanoseconds
   */
  def millisToNanos(ms: Long): Long =
    ms * NanosecondsPerMillisecond

  /**
   * Converts a duration in nanoseconds to milliseconds.
   *
   * @param ns
   *   duration in nanoseconds
   * @return
   *   the same duration expressed in milliseconds
   */
  def nanosToMillis(ns: Long): Double =
    ns.toDouble / NanosecondsPerMillisecond

  /**
   * Converts a duration in nanoseconds to integral milliseconds.
   *
   * @param ns
   *   duration in nanoseconds
   * @return
   *   the whole milliseconds contained in the supplied duration
   */
  def nanosToWholeMillis(ns: Long): Long =
    nanosToMillis(ns).toLong
