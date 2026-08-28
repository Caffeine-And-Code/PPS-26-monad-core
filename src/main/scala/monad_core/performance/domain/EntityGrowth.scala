package monad_core.performance.domain

import scala.annotation.tailrec

/**
 * Entity count progression used by stress and scalability experiments.
 *
 * Counts start at `start`, are multiplied by `factor`, and are capped at `maximum`.
 * The maximum is always included, even when it is not an exact power of the factor.
 *
 * @param start
 *   first entity count in the progression
 * @param maximum
 *   inclusive upper bound, greater than or equal to `start`
 * @param factor
 *   multiplicative growth factor, strictly greater than one
 */
final case class EntityGrowth private (
    start: EntityCount,
    maximum: EntityCount,
    factor: Int
):

  /**
   * Expands this growth policy into its finite sequence of entity counts.
   *
   * @return
   *   increasing counts from `start` through `maximum`, or a validation error
   */
  def counts: Either[PerformanceError, Vector[EntityCount]] =
    loop(start.value, Vector.empty)

  /**
   * Tail-recursive builder for [[counts]].
   *
   * @param current
   *   current entity count to append
   * @param accumulated
   *   counts already generated
   * @return
   *   the completed count sequence once `maximum` is reached, or a validation error
   */
  @tailrec
  private def loop(
      current: Int,
      accumulated: Vector[EntityCount]
  ): Either[PerformanceError, Vector[EntityCount]] =
    EntityCount.from(current) match
      case Left(error) => Left(error)
      case Right(currentCount) =>
        val nextAccumulated = accumulated :+ currentCount
        if current == maximum.value then Right(nextAccumulated)
        else
          val multiplied = current.toLong * factor.toLong
          val next       = math.min(multiplied, maximum.value.toLong).toInt
          loop(next, nextAccumulated)

object EntityGrowth:

  /**
   * Validates and creates an entity-growth policy.
   *
   * Both entity counts must be positive,
   * `maximum` must not be lower than `start`,
   * and `factor` must be greater than one.
   *
   * @param start
   *   candidate starting entity count
   * @param maximum
   *   candidate inclusive upper entity count
   * @param factor
   *   candidate multiplicative factor
   * @return
   *   a validated policy or the first corresponding [[PerformanceError]]
   */
  def from(start: Int, maximum: Int, factor: Int): Either[PerformanceError, EntityGrowth] =
    for
      startCount   <- EntityCount.from(start)
      maximumCount <- EntityCount.from(maximum)
      _ <- Either.cond(
        maximumCount.value >= startCount.value,
        (),
        InvalidGrowthMaximum(start, maximum)
      )
      _ <- Either.cond(factor > 1, (), InvalidGrowthFactor(factor))
    yield EntityGrowth(startCount, maximumCount, factor)
