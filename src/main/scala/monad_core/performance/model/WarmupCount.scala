package monad_core.performance.model

opaque type WarmupCount = Int

object WarmupCount:

  def from(value: Int): Either[PerformanceError, WarmupCount] =
    Either.cond(value >= 0, value, InvalidWarmupCount(value))

  extension (count: WarmupCount) def value: Int = count
