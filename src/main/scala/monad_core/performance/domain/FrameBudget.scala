package monad_core.performance.domain

/**
 * Maximum target duration for a single frame or workload execution.
 *
 * @param nanos
 *   strictly positive budget expressed in nanoseconds
 */
final case class FrameBudget private (nanos: Long):

  /**
   * Calculates the fraction of samples completed within this budget.
   *
   * A sample meets the budget when its duration is less than or equal to `nanos`. The result is a
   * ratio in the inclusive range `[0.0, 1.0]`.
   *
   * @param samples
   *   samples whose durations are evaluated
   * @return
   *   the completion ratio, or [[EmptyPerformanceSamples]] when `samples` is empty
   */
  def completionRate(samples: Vector[PerformanceSample]): Either[PerformanceError, Double] =
    Either.cond(
      samples.nonEmpty,
      samples.count(_.durationNanos <= nanos).toDouble / samples.size,
      EmptyPerformanceSamples()
    )

object FrameBudget:

  /**
   * Validates and creates a frame budget.
   *
   * @param nanos
   *   candidate budget in nanoseconds
   * @return
   *   the validated budget when `nanos` is strictly positive, otherwise [[InvalidFrameBudget]]
   */
  def from(nanos: Long): Either[PerformanceError, FrameBudget] =
    Either.cond(nanos > 0, FrameBudget(nanos), InvalidFrameBudget(nanos))
