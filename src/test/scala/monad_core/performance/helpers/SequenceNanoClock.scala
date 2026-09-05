package monad_core.performance.helpers

import monad_core.performance.model.NanoClock

/** Deterministic test clock that returns the supplied nanosecond values in order. */
final case class SequenceNanoClock(values: Vector[Long]) extends NanoClock:
  private var index = 0

  /**
   * Returns the next configured timestamp and advances the sequence.
   *
   * @return
   *   next deterministic nanosecond value
   */
  override def now(): Long =
    val value = values(index)
    index += 1
    value
