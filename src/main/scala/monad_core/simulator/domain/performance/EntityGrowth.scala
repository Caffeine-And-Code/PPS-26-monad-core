package monad_core.simulator.domain.performance

import scala.annotation.tailrec

/** Multiplicative factor used to increase an entity count. */
opaque type GrowthFactor = Int

object GrowthFactor:

  /**
   * Creates a growth factor greater than one.
   *
   * @param value
   *   factor to validate
   * @return
   *   the validated factor, or an error when `value` is not greater than one
   * @see
   *   [[InvalidGrowthFactor InvalidGrowthFactor]]
   */
  def from(value: Int): Either[PerformanceError, GrowthFactor] =
    Either.cond(value > 1, value, InvalidGrowthFactor(value))

  extension (factor: GrowthFactor) def value: Int = factor

/**
 * Defines the entity-count progression of a performance experiment.
 *
 * @param range
 *   interval containing the first and greatest entity counts
 * @param factor
 *   multiplier applied between consecutive counts
 */
final case class EntityGrowth private (
    range: EntityRange,
    factor: GrowthFactor
):

  /** First entity count in the progression. */
  def start: EntityCount = range.start

  /** Greatest entity count in the progression. */
  def maximum: EntityCount = range.maximum

  /**
   * Generates every entity count from `start` to `maximum`.
   *
   * The maximum is always included, even when multiplying by the factor would exceed it.
   *
   * @return
   *   the ordered entity counts, or the first validation error
   * @see
   *   [[EntityCount EntityCount]]
   */
  def counts: Either[PerformanceError, Vector[EntityCount]] =
    /**
     * Recursively appends the current count and calculates the following one.
     *
     * @param current
     *   entity count currently being added
     * @param accumulated
     *   validated counts generated so far
     * @return
     *   the completed progression, or the first validation error
     */
    @tailrec
    def generateGrowthCount(
        current: Int,
        accumulated: Vector[EntityCount]
    ): Either[PerformanceError, Vector[EntityCount]] =
      EntityCount.from(current) match
        case Left(error) => Left(error)
        case Right(count) =>
          val updated = accumulated :+ count
          if current == maximum.value then Right(updated)
          else
            val multiplied = current.toLong * factor.value.toLong
            val nextValue  = math.min(multiplied, maximum.value.toLong).toInt
            generateGrowthCount(nextValue, updated)

    generateGrowthCount(start.value, Vector.empty)

object EntityGrowth:

  /**
   * Creates an entity-count progression.
   *
   * @param start
   *   first entity count
   * @param maximum
   *   greatest entity count, which cannot be lower than `start`
   * @param factor
   *   multiplier applied between counts
   * @return
   *   the validated progression, or the first invalid argument
   * @see
   * [[performance.EntityRange EntityRange]] and
   * [[performance.GrowthFactor GrowthFactor]]
   */
  def from(
      start: Int,
      maximum: Int,
      factor: Int
  ): Either[PerformanceError, EntityGrowth] =
    for
      range        <- EntityRange.from(start, maximum)
      growthFactor <- GrowthFactor.from(factor)
    yield EntityGrowth(range, growthFactor)
