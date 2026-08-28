package monad_core.performance.simulator

import monad_core.engine.physics.core.PhysicsManager
import monad_core.performance.core.PerformanceRequest
import monad_core.performance.model.{
  InvalidPerformanceArgument,
  NanoClock,
  PerformanceConfig,
  PerformanceError,
  PerformanceKind,
  PerformancePoint,
  PerformanceReport,
  UnknownPerformanceRoute
}

import java.util.Locale

/** Command-line adapter for parsing, running, and formatting performance experiments. */
object PerformanceCli:
  val LoadRoute        = "performance-load-test"
  val StressRoute      = "performance-stress-test"
  val SpikeRoute       = "performance-spike-test"
  val ScalabilityRoute = "performance-scalability-test"

  val Entities          = "--entities"
  val MaximumEntities   = "--max-entities"
  val GrowthFactor      = "--growth-factor"
  val Iterations        = "--iterations"
  val Warmups           = "--warmups"
  val FrameBudgetMillis = "--frame-budget-ms"

  def parse(
      route: String,
      arguments: Array[String]
  ): Either[PerformanceError, PerformanceRequest] =
    for
      kind <- kindFor(route)
      start <- intArgument(
        arguments,
        Entities,
        PerformanceConfig.DefaultStartEntities
      )
      maximum <- intArgument(
        arguments,
        MaximumEntities,
        math.max(start, PerformanceConfig.DefaultMaximumEntities)
      )
      factor <- intArgument(
        arguments,
        GrowthFactor,
        PerformanceConfig.DefaultGrowthFactor
      )
      iterations <- intArgument(
        arguments,
        Iterations,
        PerformanceConfig.DefaultIterations
      )
      warmups <- intArgument(
        arguments,
        Warmups,
        PerformanceConfig.DefaultWarmups
      )
      budgetMillis <- longArgument(
        arguments,
        FrameBudgetMillis,
        PerformanceConfig.DefaultFrameBudgetMillis
      )
      config <- PerformanceConfig.from(
        start,
        maximum,
        factor,
        iterations,
        warmups,
        budgetMillis
      )
    yield PerformanceRequest(kind, config)

  def run(
      route: String,
      arguments: Array[String],
      physicsManager: PhysicsManager
  )(using clock: NanoClock): Either[PerformanceError, String] =
    for
      request <- parse(route, arguments)
      report  <- EnginePerformance.run(request, physicsManager)
    yield format(report)

  def format(report: PerformanceReport): String =
    val header     = Vector(s"Performance experiment: ${report.kind}")
    val points     = report.points.map(formatPoint)
    val breakpoint = report.breakpoint.map(count => s"Breakpoint: ${count.value} entities")

    (header ++ points ++ breakpoint).mkString("\n")

  private def kindFor(route: String): Either[PerformanceError, PerformanceKind] =
    route match
      case LoadRoute        => Right(PerformanceKind.Load)
      case StressRoute      => Right(PerformanceKind.Stress)
      case SpikeRoute       => Right(PerformanceKind.Spike)
      case ScalabilityRoute => Right(PerformanceKind.Scalability)
      case unknown          => Left(UnknownPerformanceRoute(unknown))

  private def intArgument(
      arguments: Array[String],
      name: String,
      default: Int
  ): Either[PerformanceError, Int] =
    argument(arguments, name).fold(Right(default): Either[PerformanceError, Int]) { value =>
      value.toIntOption.toRight(InvalidPerformanceArgument(name, value))
    }

  private def longArgument(
      arguments: Array[String],
      name: String,
      default: Long
  ): Either[PerformanceError, Long] =
    argument(arguments, name).fold(Right(default): Either[PerformanceError, Long]) { value =>
      value.toLongOption.toRight(InvalidPerformanceArgument(name, value))
    }

  private def argument(arguments: Array[String], name: String): Option[String] =
    arguments.indexOf(name) match
      case index if index >= 0 && index + 1 < arguments.length => Some(arguments(index + 1))
      case _                                                   => None

  private def formatPoint(point: PerformancePoint): String =
    Vector(
      s"Entities: ${point.entityCount.value}",
      s"p50: ${milliseconds(point.latency.p50Nanos)}",
      s"p95: ${milliseconds(point.latency.p95Nanos)}",
      s"p99: ${milliseconds(point.latency.p99Nanos)}",
      s"Frame budget completion: ${percentage(point.frameBudgetCompletionRate)}"
    ).mkString("\n")

  private def milliseconds(nanos: Long): String =
    String.format(
      Locale.ROOT,
      "%.3f ms",
      Double.box(nanos.toDouble / 1_000_000.0)
    )

  private def percentage(rate: Double): String =
    String.format(
      Locale.ROOT,
      "%.2f%%",
      Double.box(rate * 100.0)
    )
