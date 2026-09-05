package monad_core.performance.model

import scala.annotation.tailrec

/** Multiplicative factor used to increase an entity count. */
opaque type GrowthFactor = Int

/** Provides validated construction and operations for growth factors. */
object GrowthFactor:

  /**
   * Creates a growth factor greater than one.
   *
   * @param value
   *   factor to validate
   * @return
   *   the validated factor, or an error when `value` is not greater than one
   * @see
   *   [[monad_core.performance.model.InvalidGrowthFactor InvalidGrowthFactor]]
   */
  def from(value: Int): Either[PerformanceError, GrowthFactor] =
    Either.cond(value > 1, value, InvalidGrowthFactor(value))

  /**
   * Returns the integer represented by a validated growth factor.
   *
   * @param factor
   *   validated growth factor
   * @return
   *   underlying integer value
   */
  extension (factor: GrowthFactor) def value: Int = factor

/**
 * Defines the entity-count progression of a performance experiment.
 *
 * @param start
 *   first entity count
 * @param maximum
 *   greatest entity count
 * @param factor
 *   multiplier applied between consecutive counts
 */
final private[performance] case class EntityGrowth private (
    start: EntityCount,
    maximum: EntityCount,
    factor: GrowthFactor
):

  /**
   * Generates every entity count from `start` to `maximum`.
   *
   * The maximum is always included, even when multiplying by the factor would exceed it.
   *
   * @return
   *   the ordered entity counts, or the first validation error
   * @see
   *   [[monad_core.performance.model.EntityCount EntityCount]]
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

/** Provides validated construction for entity-count progressions. */
private[performance] object EntityGrowth:

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
   *   [[monad_core.performance.model.EntityCount EntityCount]] and
   *   [[monad_core.performance.model.GrowthFactor GrowthFactor]]
   */
  def from(
      start: Int,
      maximum: Int,
      factor: Int
  ): Either[PerformanceError, EntityGrowth] =
    for
      startCount   <- EntityCount.from(start)
      maximumCount <- EntityCount.from(maximum)
      _ <- Either.cond(
        maximumCount.value >= startCount.value,
        (),
        InvalidGrowthMaximum(start, maximum)
      )
      growthFactor <- GrowthFactor.from(factor)
    yield EntityGrowth(startCount, maximumCount, growthFactor)
