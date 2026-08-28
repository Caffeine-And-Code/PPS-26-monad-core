package monad_core.performance.helpers

import monad_core.performance.application.NanoClock

final case class SequenceNanoClock(values: Vector[Long]) extends NanoClock:
  private var index = 0

  override def now(): Long =
    val value = values(index)
    index += 1
    value
