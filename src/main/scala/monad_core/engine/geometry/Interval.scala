package monad_core.engine.geometry

/**
 * Closed numeric interval containing both endpoints.
 *
 * @param min lower endpoint
 * @param max upper endpoint
 */
final case class Interval(min: Double, max: Double)

/** Provides operations for [[Interval]]. */
object Interval:

  extension (interval: Interval)

    /**
     * Checks whether a value belongs to this closed interval.
     *
     * @param value value to test
     * @return `true` when `min <= value <= max`
     */
    infix def contains(value: Double): Boolean =
      interval.min <= value && value <= interval.max
