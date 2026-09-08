package monad_core.simulator.infrastructure.performance

import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.engine.simulator.EngineFacade.PhysicsRuleStatus
import monad_core.simulator.domain.performance.{PerformanceKind, PerformancePlan}
import monad_core.simulator.application.performance.PerformanceRequest
import monad_core.simulator.domain.performance.{
  InvalidPerformanceArgument,
  NanoClock,
  PerformanceConfig,
  PerformanceError,
  PerformanceKind,
  PerformancePlan,
  PerformancePoint,
  PerformanceReport,
  UnknownPerformanceRoute
}

import java.util.Locale

/**
 * Command-line adapter for parsing, running and formatting performance experiments.
 *
 * @see
 *   [[EnginePerformance EnginePerformance]]
 */
object PerformanceCli:
  /** Route that runs the expected-load strategy. */
  val LoadRoute = "performance-load-test"

  /** Route that searches for the frame-budget breakpoint. */
  val StressRoute = "performance-stress-test"

  /** Route that introduces a sudden increase and recovery in entity count. */
  val SpikeRoute = "performance-spike-test"

  /** Route that measures the complete entity-count progression. */
  val ScalabilityRoute = "performance-scalability-test"

  /** Argument selecting the initial number of entities. */
  val Entities = "--entities"

  /** Argument selecting the maximum number of entities. */
  val MaximumEntities = "--max-entities"

  /** Argument selecting the multiplier between entity counts. */
  val GrowthFactor = "--growth-factor"

  /** Argument selecting the measured executions for each entity count. */
  val Iterations = "--iterations"

  /** Argument selecting the unmeasured executions before collection. */
  val Warmups = "--warmups"

  /** Argument selecting the frame budget in milliseconds. */
  val FrameBudgetMillis = "--frame-budget-ms"

  /** Default initial entity count exposed to command-line clients. */
  val DefaultStartEntities: Int = PerformancePlan.DefaultStartEntities

  /** Default maximum entity count exposed to command-line clients. */
  val DefaultMaximumEntities: Int = PerformancePlan.DefaultMaximumEntities

  /** Default entity growth factor exposed to command-line clients. */
  val DefaultGrowthFactor: Int = PerformancePlan.DefaultGrowthFactor

  /** Default measured iteration count exposed to command-line clients. */
  val DefaultIterations: Int = PerformanceConfig.DefaultIterations

  /** Default warm-up count exposed to command-line clients. */
  val DefaultWarmups: Int = PerformanceConfig.DefaultWarmups

  /** Default frame budget in milliseconds exposed to command-line clients. */
  val DefaultFrameBudgetMillis: Long = PerformanceConfig.DefaultFrameBudgetMillis

  /**
   * Parses a route and its command-line arguments into a validated request.
   *
   * Missing relevant arguments use their default values. A supplied argument without a following
   * value is treated as missing, while arguments unused by the selected route are ignored.
   *
   * @param route
   *   selected performance route
   * @param arguments
   *   command-line option and value pairs
   * @return
   *   the validated request, or the first route, parsing or configuration error
   * @see
   * [[PerformancePlan PerformancePlan]] and
   * [[PerformanceConfig PerformanceConfig]]
   */
  def parse(
      route: String,
      arguments: Array[String]
  ): Either[PerformanceError, PerformanceRequest] =
    for
      kind <- kindFor(route)
      plan <- planFor(kind, arguments)
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
        iterations,
        warmups,
        budgetMillis
      )
    yield PerformanceRequest(plan, config)

  /**
   * Runs an engine performance command using the system monotonic clock.
   *
   * @param route
   *   selected performance route
   * @param arguments
   *   command-line arguments
   * @param physicsManager
   *   physics rules applied to the deterministic scene
   * @return
   *   the formatted report, or the first validation or engine error
   * @see
   *   [[PerformanceClock PerformanceClock]]
   */
  def run(
      route: String,
      arguments: Array[String],
      physicsManager: PhysicsManager
  ): Either[PerformanceError, String] =
    runWithClock(route, arguments, physicsManager)(using PerformanceClock)

  /**
   * Runs an engine performance command with the default physics configuration.
   *
   * @param route
   *   selected performance route
   * @param arguments
   *   command-line arguments
   * @return
   *   the formatted report, or the first validation or engine error
   */
  def run(
      route: String,
      arguments: Array[String]
  ): Either[PerformanceError, String] =
    run(route, arguments, PhysicsManager.default())

  /**
   * Runs an engine performance command with the supplied public rule states.
   *
   * @param route
   *   selected performance route
   * @param arguments
   *   command-line arguments
   * @param rules
   *   enabled state of the runtime's configurable physics rules
   * @return
   *   the formatted report, or the first validation or engine error
   * @see
   *   [[monad_core.engine.simulator.EngineFacade.PhysicsRuleStatus PhysicsRuleStatus]]
   */
  def runWithRules(
      route: String,
      arguments: Array[String],
      rules: Vector[PhysicsRuleStatus]
  ): Either[PerformanceError, String] =
    val physicsManager = rules.foldLeft(PhysicsManager.default()): (physics, rule) =>
      EngineFacade.setPhysicsRuleEnabled(physics, rule.id, rule.isEnabled)

    run(route, arguments, physicsManager)

  /**
   * Runs an engine performance command using a specific clock.
   *
   * @param route
   *   selected performance route
   * @param arguments
   *   command-line arguments
   * @param physicsManager
   *   physics rules applied to the deterministic scene
   * @param clock
   *   monotonic clock used for timing
   * @return
   *   the formatted report, or the first validation or engine error
   * @see
   *   [[EnginePerformance EnginePerformance]]
   */
  def runWithClock(
      route: String,
      arguments: Array[String],
      physicsManager: PhysicsManager
  )(using clock: NanoClock): Either[PerformanceError, String] =
    for
      request <- parse(route, arguments)
      report  <- EnginePerformance.run(request, physicsManager)
    yield format(report)

  /**
   * Formats a report as a line-oriented textual result.
   *
   * @param report
   *   report to format
   * @return
   *   experiment header, measurement points and optional breakpoint
   */
  def format(report: PerformanceReport): String =
    val header     = Vector(s"Performance experiment: ${report.kind}")
    val points     = report.points.map(formatPoint)
    val breakpoint = report.breakpoint.map(count => s"Breakpoint: ${count.value} entities")

    (header ++ points ++ breakpoint).mkString("\n")

  /**
   * Resolves a command-line route to its performance strategy.
   *
   * @param route
   *   route to resolve
   * @return
   *   the corresponding strategy, or an unknown-route error
   * @see
   *   [[UnknownPerformanceRoute UnknownPerformanceRoute]]
   */
  private def kindFor(route: String): Either[PerformanceError, PerformanceKind] =
    route match
      case LoadRoute        => Right(PerformanceKind.Load)
      case StressRoute      => Right(PerformanceKind.Stress)
      case SpikeRoute       => Right(PerformanceKind.Spike)
      case ScalabilityRoute => Right(PerformanceKind.Scalability)
      case unknown          => Left(UnknownPerformanceRoute(unknown))

  /**
   * Parses the entity-count values used by the selected strategy.
   *
   * @param kind
   *   strategy whose plan is being created
   * @param arguments
   *   command-line option and value pairs
   * @return
   *   the validated plan, or the first invalid relevant argument
   * @see
   *   [[PerformancePlan PerformancePlan]]
   */
  private def planFor(
      kind: PerformanceKind,
      arguments: Array[String]
  ): Either[PerformanceError, PerformancePlan] =
    intArgument(arguments, Entities, PerformancePlan.DefaultStartEntities).flatMap { start =>
      kind match
        case PerformanceKind.Load => PerformancePlan.load(start)
        case PerformanceKind.Spike =>
          for
            maximum <- intArgument(
              arguments,
              MaximumEntities,
              math.max(start, PerformancePlan.DefaultMaximumEntities)
            )
            plan <- PerformancePlan.spike(start, maximum)
          yield plan
        case PerformanceKind.Stress =>
          growingPlan(arguments, start, PerformancePlan.stress)
        case PerformanceKind.Scalability =>
          growingPlan(arguments, start, PerformancePlan.scalability)
    }

  /**
   * Parses the values shared by stress and scalability growth plans.
   *
   * @param arguments
   *   command-line option and value pairs
   * @param start
   *   already parsed initial entity count
   * @param create
   *   validated constructor for the selected growth plan
   * @return
   *   the validated plan, or the first invalid relevant argument
   */
  private def growingPlan(
      arguments: Array[String],
      start: Int,
      create: (Int, Int, Int) => Either[PerformanceError, PerformancePlan]
  ): Either[PerformanceError, PerformancePlan] =
    for
      maximum <- intArgument(
        arguments,
        MaximumEntities,
        math.max(start, PerformancePlan.DefaultMaximumEntities)
      )
      factor <- intArgument(
        arguments,
        GrowthFactor,
        PerformancePlan.DefaultGrowthFactor
      )
      plan <- create(start, maximum, factor)
    yield plan

  /**
   * Reads an integer argument or returns its default value.
   *
   * @param arguments
   *   command-line option and value pairs
   * @param name
   *   option to find
   * @param default
   *   value used when the option is absent
   * @return
   *   the parsed integer, its default, or an invalid-argument error
   * @see
   *   [[InvalidPerformanceArgument InvalidPerformanceArgument]]
   */
  private def intArgument(
      arguments: Array[String],
      name: String,
      default: Int
  ): Either[PerformanceError, Int] =
    argument(arguments, name).fold(Right(default): Either[PerformanceError, Int]) { value =>
      value.toIntOption.toRight(InvalidPerformanceArgument(name, value))
    }

  /**
   * Reads a long argument or returns its default value.
   *
   * @param arguments
   *   command-line option and value pairs
   * @param name
   *   option to find
   * @param default
   *   value used when the option is absent
   * @return
   *   the parsed long, its default, or an invalid-argument error
   * @see
   *   [[InvalidPerformanceArgument InvalidPerformanceArgument]]
   */
  private def longArgument(
      arguments: Array[String],
      name: String,
      default: Long
  ): Either[PerformanceError, Long] =
    argument(arguments, name).fold(Right(default): Either[PerformanceError, Long]) { value =>
      value.toLongOption.toRight(InvalidPerformanceArgument(name, value))
    }

  /**
   * Finds the value immediately following the first occurrence of an option.
   *
   * @param arguments
   *   command-line option and value pairs
   * @param name
   *   option to find
   * @return
   *   the following value, or `None` when the option or its value is absent
   */
  private def argument(arguments: Array[String], name: String): Option[String] =
    arguments.indexOf(name) match
      case index if index >= 0 && index + 1 < arguments.length => Some(arguments(index + 1))
      case _                                                   => None

  /**
   * Formats every metric collected for one entity count.
   *
   * @param point
   *   measurement point to format
   * @return
   *   multiline textual representation of the point
   */
  private def formatPoint(point: PerformancePoint): String =
    Vector(
      s"Entities: ${point.entityCount.value}",
      s"p50: ${milliseconds(point.latency.p50Nanos)}",
      s"p95: ${milliseconds(point.latency.p95Nanos)}",
      s"p99: ${milliseconds(point.latency.p99Nanos)}",
      s"Frame budget completion: ${percentage(point.frameBudgetCompletionRate)}"
    ).mkString("\n")

  /**
   * Formats a nanosecond duration as milliseconds with three decimal places.
   *
   * @param nanos
   *   duration in nanoseconds
   * @return
   *   locale-independent millisecond representation
   * @see
   *   [[java.util.Locale Locale]]
   */
  private def milliseconds(nanos: Long): String =
    String.format(
      Locale.ROOT,
      "%.3f ms",
      Double.box(nanos.toDouble / 1_000_000.0)
    )

  /**
   * Formats a completion rate as a percentage with two decimal places.
   *
   * @param rate
   *   completion rate between zero and one
   * @return
   *   locale-independent percentage representation
   * @see
   *   [[java.util.Locale Locale]]
   */
  private def percentage(rate: Double): String =
    String.format(
      Locale.ROOT,
      "%.2f%%",
      Double.box(rate * 100.0)
    )
