package monad_core.performance.presentation

import monad_core.performance.domain.*

/**
 * Parses command-line options into validated performance configuration.
 *
 * Options use a `--name value` representation. Missing options fall back to
 * [[PerformanceConfig]] defaults, while malformed numeric values and
 * invalid domain combinations are returned as
 * [[PerformanceError]] values.
 */
object PerformanceArguments:

  /** Option selecting the starting entity count. */
  val Entities = "--entities"

  /** Option selecting the (inclusive) maximum entity count. */
  val MaximumEntities = "--max-entities"

  /** Option selecting the multiplicative entity-growth factor. */
  val GrowthFactor = "--growth-factor"

  /** Option selecting the number of measured executions per point. */
  val Iterations = "--iterations"

  /** Option selecting the number of unmeasured executions per point. */
  val Warmups = "--warmups"

  /** Option selecting the frame budget in milliseconds. */
  val FrameBudgetMillis = "--frame-budget-ms"

  /**
   * Parses all supported arguments and validates the resulting configuration.
   *
   * If the maximum is omitted, its default is raised to the starting count when necessary. The
   * frame-budget option is converted from milliseconds to nanoseconds before domain validation.
   * Unknown arguments are ignored.
   *
   * @param args
   *   raw command-line tokens
   * @return
   *   a validated performance configuration, a numeric parsing error, or a domain validation error
   */
  def parse(args: Array[String]): Either[PerformanceError, PerformanceConfig] =
    for
      start <- integerArgument(args, Entities, PerformanceConfig.DefaultStartEntities)
      maximum <- integerArgument(
        args,
        MaximumEntities,
        math.max(start, PerformanceConfig.DefaultMaximumEntities)
      )
      factor     <- integerArgument(args, GrowthFactor, PerformanceConfig.DefaultGrowthFactor)
      iterations <- integerArgument(args, Iterations, PerformanceConfig.DefaultIterations)
      warmups    <- integerArgument(args, Warmups, PerformanceConfig.DefaultWarmups)
      budgetMillis <- longArgument(
        args,
        FrameBudgetMillis,
        DurationConversion.nanosToWholeMillis(
          PerformanceConfig.DefaultFrameBudgetNanos
        )
      )
      config <- PerformanceConfig.from(
        startEntities = start,
        maximumEntities = maximum,
        growthFactor = factor,
        iterations = iterations,
        warmups = warmups,
        frameBudgetNanos = DurationConversion.millisToNanos(budgetMillis)
      )
    yield config

  /**
   * Reads an optional integer-valued command-line option.
   *
   * @param args
   *   complete command-line token array
   * @param name
   *   option name to locate
   * @param default
   *   value returned when the option is absent or lacks a following token
   * @return
   *   the parsed integer or [[InvalidPerformanceArgument]] for a noninteger token
   */
  private def integerArgument(
      args: Array[String],
      name: String,
      default: Int
  ): Either[PerformanceError, Int] =
    argument(args, name).fold(Right(default): Either[PerformanceError, Int]) { value =>
      value.toIntOption.toRight(InvalidPerformanceArgument(name, value))
    }

  /**
   * Reads an optional long-valued command-line option.
   *
   * @param args
   *   complete command-line token array
   * @param name
   *   option name to locate
   * @param default
   *   value returned when the option is absent or lacks a following token
   * @return
   *   the parsed long or [[InvalidPerformanceArgument]] for a non-long token
   */
  private def longArgument(
      args: Array[String],
      name: String,
      default: Long
  ): Either[PerformanceError, Long] =
    argument(args, name).fold(Right(default): Either[PerformanceError, Long]) { value =>
      value.toLongOption.toRight(InvalidPerformanceArgument(name, value))
    }

  /**
   * Locates the token immediately following the first occurrence of an option.
   *
   * @param args
   *   complete command-line token array
   * @param name
   *   exact option name to locate
   * @return
   *   the following token, or `None` when the option is absent or appears last
   */
  private def argument(args: Array[String], name: String): Option[String] =
    args.indexOf(name) match
      case index if index >= 0 && index + 1 < args.length => Some(args(index + 1))
      case _                                              => None
