package monad_core.performance.core

import monad_core.performance.model.{EntityCount, NanoClock, PerformanceConfig, PerformanceError, PerformanceKind, PerformancePoint, PerformanceReport}

import scala.annotation.tailrec

/**
 * Performance experiment selected for execution.
 *
 * @param kind
 *   strategy used to generate the entity counts to measure
 * @param config
 *   shared experiment configuration
 */
final case class PerformanceRequest(kind: PerformanceKind, config: PerformanceConfig)

/** Executes the selected performance strategy over an injected workload. */
private[performance] object PerformanceRunner:
  /** Workload operation executed during warm-ups and measured iterations. */
  type Operation       = () => Either[PerformanceError, Unit]

  /** Creates a workload operation for the supplied number of entities. */
  type PrepareWorkload = EntityCount => Either[PerformanceError, Operation]

  /**
   * Runs a performance experiment and collects its measurements.
   *
   * Load measures the starting count, Spike measures the starting and maximum counts before
   * returning to the starting count, while Stress and Scalability use every generated count.
   * Stress stops at the first point whose p95 latency exceeds the frame budget.
   *
   * @param request
   *   experiment strategy and configuration
   * @param prepare
   *   function that creates the workload for an entity count
   * @param clock
   *   monotonic clock used to measure each execution
   * @return
   *   the completed report, or the first configuration or workload error
   * @see
   *   [[monad_core.performance.model.PerformanceKind PerformanceKind]] and
   *   [[monad_core.performance.model.PerformanceReport PerformanceReport]]
   */
  def run(
      request: PerformanceRequest,
      prepare: PrepareWorkload
  )(using clock: NanoClock): Either[PerformanceError, PerformanceReport] =
    val counts = request.kind match
      case PerformanceKind.Load =>
        Right(Vector(request.config.growth.start))
      case PerformanceKind.Spike =>
        Right(
          Vector(
            request.config.growth.start,
            request.config.growth.maximum,
            request.config.growth.start
          )
        )
      case PerformanceKind.Stress | PerformanceKind.Scalability =>
        request.config.growth.counts

    counts.flatMap(countValues =>
      collect(
        countValues,
        request,
        prepare,
        stopAtBreakpoint = request.kind == PerformanceKind.Stress
      ).map { points =>
        PerformanceReport(
          request.kind,
          points,
          if request.kind == PerformanceKind.Stress then
            points
              .find(_.latency.p95Nanos > request.config.frameBudget.toNanos)
              .map(_.entityCount)
          else None
        )
      }
    )

  /**
   * Measures the remaining entity counts in order.
   *
   * @param remaining
   *   entity counts that have not been measured yet
   * @param request
   *   current experiment request
   * @param prepare
   *   function that creates the workload for an entity count
   * @param stopAtBreakpoint
   *   whether collection stops when p95 exceeds the frame budget
   * @param accumulated
   *   points already measured
   * @param clock
   *   monotonic clock used to measure each execution
   * @return
   *   all collected points, or the first workload error
   * @see
   *   [[monad_core.performance.model.PerformancePoint PerformancePoint]]
   */
  @tailrec
  private def collect(
      remaining: Vector[EntityCount],
      request: PerformanceRequest,
      prepare: PrepareWorkload,
      stopAtBreakpoint: Boolean,
      accumulated: Vector[PerformancePoint] = Vector.empty
  )(using clock: NanoClock): Either[PerformanceError, Vector[PerformancePoint]] =
    remaining.headOption match
      case None => Right(accumulated)
      case Some(entityCount) =>
        measure(entityCount, request.config, prepare) match
          case Left(error) => Left(error)
          case Right(point) =>
            val updated = accumulated :+ point
            if stopAtBreakpoint && point.latency.p95Nanos > request.config.frameBudget.toNanos then
              Right(updated)
            else
              collect(
                remaining.tail,
                request,
                prepare,
                stopAtBreakpoint,
                updated
              )

  /**
   * Prepares and measures the workload for one entity count.
   *
   * Warm-ups are executed before collecting the configured latency samples.
   *
   * @param entityCount
   *   number of entities used by the workload
   * @param config
   *   experiment configuration
   * @param prepare
   *   function that creates the workload
   * @param clock
   *   monotonic clock used to measure each execution
   * @return
   *   the resulting performance point, or the first workload error
   * @see
   *   [[monad_core.performance.core.PerformanceMetrics PerformanceMetrics]]
   */
  private def measure(
      entityCount: EntityCount,
      config: PerformanceConfig,
      prepare: PrepareWorkload
  )(using clock: NanoClock): Either[PerformanceError, PerformancePoint] =
    for
      operation <- prepare(entityCount)
      _         <- repeat(config.warmups.value, operation, Vector.empty).map(_ => ())
      samples   <- repeat(config.iterations.value, measured(operation), Vector.empty)
      latency   <- PerformanceMetrics.latency(samples)
      rate      <- PerformanceMetrics.completionRate(samples, config.frameBudget)
    yield PerformancePoint(entityCount, latency, rate)

  /**
   * Decorates a workload operation with elapsed-time measurement.
   *
   * @param operation
   *   workload operation to execute
   * @param clock
   *   monotonic clock read before and after the operation
   * @return
   *   an operation producing the collected performance sample
   * @see
   *   [[monad_core.performance.model.NanoClock NanoClock]]
   */
  private def measured(
      operation: Operation
  )(using clock: NanoClock): () => Either[PerformanceError, PerformanceSample] = () =>
    val startedAt = clock.now()
    operation().map(_ => PerformanceSample(clock.now() - startedAt))

  /**
   * Executes an operation a fixed number of times, stopping at the first error.
   *
   * @tparam A
   *   value produced by one execution
   * @param remaining
   *   number of executions still required
   * @param operation
   *   operation to repeat
   * @param accumulated
   *   values produced by completed executions
   * @return
   *   all produced values, or the first execution error
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
