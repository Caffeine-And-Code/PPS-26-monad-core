package monad_core.engine.geometry

final case class Interval(min: Double, max: Double)

object Interval:

  extension (interval: Interval)
    infix def contains(value: Double): Boolean =
      interval.min <= value && value <= interval.max