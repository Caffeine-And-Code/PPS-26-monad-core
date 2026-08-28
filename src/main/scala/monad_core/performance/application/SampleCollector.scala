package monad_core.performance.application

import monad_core.performance.domain.*

import scala.annotation.tailrec

/**
 * Executes performance operations for warm-up and latency sampling.
 *
 * Operations are run sequentially and collection stops at the first domain error. Only measured
 * executions read the configured [[NanoClock]]; warm-up executions intentionally discard their
 * results and timings.
 */
object SampleCollector:

  /** A single fallible unit of work executed by the collector. */
  type Operation = () => Either[PerformanceError, Unit]

  /**
   * Runs an operation repeatedly without measuring it.
   *
   * @param warmups
   *   validated number of warm-up executions
   * @param operation
   *   operation to execute
   * @return
   *   `Right(())` after every execution succeeds, or the first error returned by `operation`
   */
  def warmUp(warmups: WarmupCount, operation: Operation): Either[PerformanceError, Unit] =
    repeat(warmups.value, operation, Vector.empty).map(_ => ())

  /**
   * Measures the duration of repeated operation executions.
   *
   * Each sample spans one invocation of `operation`, from the clock reading immediately before
   * the invocation to the reading immediately after a successful result. If an invocation fails,
   * collection stops and no partial sample vector is returned.
   *
   * @param iterations
   *   validated number of samples to collect
   * @param operation
   *   operation whose latency is measured
   * @param clock
   *   monotonic nanosecond clock used for elapsed-time calculation
   * @return
   *   samples in execution order, or the first error returned by `operation`
   */
  def collect(iterations: IterationCount, operation: Operation)(using
      clock: NanoClock
  ): Either[PerformanceError, Vector[PerformanceSample]] =
    repeat(iterations.value, measured(operation), Vector.empty)

  /**
   * Decorates an operation with nanosecond timing.
   *
   * @param operation
   *   operation to measure
   * @param clock
   *   monotonic clock read immediately before and after the operation
   * @return
   *   an operation that yields the elapsed time when the wrapped operation succeeds
   */
  private def measured(
      operation: Operation
  )(using clock: NanoClock): () => Either[PerformanceError, PerformanceSample] = () =>
    val startedAt = clock.now()
    operation().map { _ =>
      PerformanceSample(clock.now() - startedAt)
    }

  /**
   * Executes an operation a fixed number of times and accumulates successful results.
   *
   * @param remaining
   *   number of executions still to perform; callers must supply a non-negative value
   * @param operation
   *   fallible operation executed at each step
   * @param accumulated
   *   results already produced, in execution order
   * @tparam A
   *   type returned by a successful operation
   * @return
   *   all accumulated results, or the first encountered error
   */
  @tailrec
  private def repeat[A](
      remaining: Int,
      operation: () => Either[PerformanceError, A],
      accumulated: Vector[A]
  ): Either[PerformanceError, Vector[A]] =
    if remaining == 0 then Right(accumulated)
    else
      operation() match
        case Left(error)  => Left(error)
        case Right(value) => repeat(remaining - 1, operation, accumulated :+ value)
