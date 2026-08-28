package monad_core.performance.core

import monad_core.performance.model.{EntityCount, NanoClock, PerformanceConfig, PerformanceError, PerformanceKind, PerformancePoint, PerformanceReport}

import scala.annotation.tailrec

final case class PerformanceRequest(kind: PerformanceKind, config: PerformanceConfig)

/** Executes the four performance strategies over an injected workload. */
object PerformanceRunner:
  type Operation       = () => Either[PerformanceError, Unit]
  type PrepareWorkload = EntityCount => Either[PerformanceError, Operation]

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

  private def measured(
      operation: Operation
  )(using clock: NanoClock): () => Either[PerformanceError, PerformanceSample] = () =>
    val startedAt = clock.now()
    operation().map(_ => PerformanceSample(clock.now() - startedAt))

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
