package monad_core.performance.simulator

import monad_core.performance.model.NanoClock

object PerformanceClock extends NanoClock:
  override def now(): Long = System.nanoTime()
