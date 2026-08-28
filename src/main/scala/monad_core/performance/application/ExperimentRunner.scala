package monad_core.performance.application

import monad_core.performance.domain.*

import scala.annotation.tailrec

/**
 * Orchestrates performance experiments and converts raw measurements into reports.
 *
 * The selected [[monad_core.performance.domain.ExperimentKind]] determines which entity counts
 * are measured and whether execution stops when the configured p95 frame budget is exceeded.
 * Workload setup, warm-up, collection, and statistical aggregation are performed once per point.
 */
object ExperimentRunner:

  /**
   * Runs a complete performance experiment.
   *
   * Load measures only the configured starting count; stress follows the growth sequence and stops
   * at its first p95 budget violation; spike measures start, maximum, and start again; scalability
   * measures the full growth sequence without early termination.
   *
   * @param kind
   *   experiment strategy to execute
   * @param config
   *   validated workload sizes, iteration counts, warm-ups, and frame budget
   * @param workload
   *   workload implementation prepared for each entity count
   * @param clock
   *   monotonic clock used to collect latency samples
   * @return
   *   the completed report, or the first preparation, execution, or aggregation error
   */
  def run(kind: ExperimentKind, config: PerformanceConfig)(using
      workload: PerformanceWorkload,
      clock: NanoClock
  ): Either[PerformanceError, ExperimentReport] =
    kind match
      case ExperimentKind.Load =>
        report(kind, Vector(config.growth.start), config, stopAtBreakpoint = false)
      case ExperimentKind.Stress =>
        config.growth.counts.flatMap(counts =>
          report(kind, counts, config, stopAtBreakpoint = true)
        )
      case ExperimentKind.Spike =>
        report(
          kind,
          Vector(config.growth.start, config.growth.maximum, config.growth.start),
          config,
          stopAtBreakpoint = false
        )
      case ExperimentKind.Scalability =>
        config.growth.counts.flatMap(counts =>
          report(kind, counts, config, stopAtBreakpoint = false)
        )

  /**
   * Collects performance points and builds the final report.
   *
   * @param kind
   *   kind recorded in the report
   * @param counts
   *   entity counts to measure, in execution order
   * @param config
   *   experiment configuration applied to every point
   * @param stopAtBreakpoint
   *   whether collection should end at the first p95 value above the frame budget
   * @param workload
   *   workload implementation used for measurements
   * @param clock
   *   clock used for latency collection
   * @return
   *   a report containing all collected points and the optional stress breakpoint
   */
  private def report(
      kind: ExperimentKind,
      counts: Vector[EntityCount],
      config: PerformanceConfig,
      stopAtBreakpoint: Boolean
  )(using
      workload: PerformanceWorkload,
      clock: NanoClock
  ): Either[PerformanceError, ExperimentReport] =
    collectPoints(counts, config, stopAtBreakpoint).map { points =>
      ExperimentReport(
        kind = kind,
        points = points,
        breakpoint =
          if stopAtBreakpoint then
            points.find(_.latency.p95Nanos > config.frameBudget.nanos).map(_.entityCount)
          else None
      )
    }

  /**
   * Measures entity counts sequentially until exhausted, failed, or stopped at a breakpoint.
   *
   * @param remaining
   *   entity counts not yet measured
   * @param config
   *   experiment configuration applied to each count
   * @param stopAtBreakpoint
   *   whether a p95 budget violation terminates collection
   * @param accumulated
   *   points already measured, in execution order
   * @param workload
   *   workload implementation used for each point
   * @param clock
   *   clock used for latency measurements
   * @return
   *   all collected points, or the first measurement error
   */
  @tailrec
  private def collectPoints(
      remaining: Vector[EntityCount],
      config: PerformanceConfig,
      stopAtBreakpoint: Boolean,
      accumulated: Vector[PerformancePoint] = Vector.empty
  )(using
      workload: PerformanceWorkload,
      clock: NanoClock
  ): Either[PerformanceError, Vector[PerformancePoint]] =
    remaining.headOption match
      case None => Right(accumulated)
      case Some(entityCount) =>
        measure(entityCount, config) match
          case Left(error) => Left(error)
          case Right(point) =>
            val updated = accumulated :+ point
            if stopAtBreakpoint && point.latency.p95Nanos > config.frameBudget.nanos then
              Right(updated)
            else collectPoints(remaining.tail, config, stopAtBreakpoint, updated)

  /**
   * Produces one aggregate performance point for an entity count.
   *
   * Workload preparation happens before warm-up and sampling. The resulting samples are used both
   * for percentile calculation and for frame-budget completion rate.
   *
   * @param entityCount
   *   workload size to measure
   * @param config
   *   warm-up, sampling, and frame-budget settings
   * @param workload
   *   workload to prepare and execute
   * @param clock
   *   clock used during sample collection
   * @return
   *   the aggregated point, or the first error produced by any stage
   */
  private def measure(entityCount: EntityCount, config: PerformanceConfig)(using
      workload: PerformanceWorkload,
      clock: NanoClock
  ): Either[PerformanceError, PerformancePoint] =
    for
      operation <- workload.prepare(entityCount)
      _         <- SampleCollector.warmUp(config.warmups, operation)
      samples   <- SampleCollector.collect(config.iterations, operation)
      latency   <- LatencyDistribution.from(samples)
      rate      <- config.frameBudget.completionRate(samples)
    yield PerformancePoint(entityCount, latency, rate)
