package monad_core.performance.model

import monad_core.engine.model.EngineError
import monad_core.simulator.errors.BaseError

/**
 * Base type for errors produced while configuring or running performance experiments.
 *
 * @param message
 *   human-readable error description
 * @see
 *   [[monad_core.simulator.errors.BaseError BaseError]]
 */
sealed abstract class PerformanceError(message: String) extends BaseError(message)

/**
 * Invalid value supplied to a count that must be greater than zero.
 *
 * @param name
 *   name of the invalid count
 * @param value
 *   rejected value
 */
final case class InvalidPositiveCount(name: String, value: Int)
  extends PerformanceError(s"$name must be positive: $value")

/**
 * Invalid number of warm-up executions.
 *
 * @param value
 *   rejected negative count
 */
final case class InvalidWarmupCount(value: Int)
  extends PerformanceError(s"Warm-up count cannot be negative: $value")

/**
 * Invalid multiplier for an entity-count progression.
 *
 * @param value
 *   rejected factor
 */
final case class InvalidGrowthFactor(value: Int)
  extends PerformanceError(s"Growth factor must be greater than one: $value")

/**
 * Maximum entity count lower than the starting count.
 *
 * @param start
 *   starting entity count
 * @param maximum
 *   rejected maximum entity count
 */
final case class InvalidGrowthMaximum(start: Int, maximum: Int)
  extends PerformanceError(s"Maximum entity count $maximum cannot be lower than start $start")

/**
 * Invalid non-positive frame budget expressed in milliseconds.
 *
 * @param value
 *   rejected frame budget
 */
final case class InvalidFrameBudget(value: Long)
  extends PerformanceError(s"Frame budget must be positive: $value")

/**
 * Invalid value associated with a command-line performance argument.
 *
 * @param argument
 *   argument name
 * @param value
 *   rejected textual value
 */
final case class InvalidPerformanceArgument(argument: String, value: String)
  extends PerformanceError(s"Invalid value '$value' for argument '$argument'")

/**
 * Command-line route that does not identify a supported performance strategy.
 *
 * @param route
 *   unsupported route
 */
final case class UnknownPerformanceRoute(route: String)
  extends PerformanceError(s"Unknown performance route: $route")

/** Missing samples when at least one measurement is required. */
final case class EmptyPerformanceSamples()
  extends PerformanceError("At least one performance sample is required")

/**
 * Error raised by the engine workload executed during a performance experiment.
 *
 * @param cause
 *   original engine error
 * @see
 *   [[monad_core.engine.model.EngineError EngineError]]
 */
final case class EnginePerformanceError(cause: EngineError)
  extends PerformanceError(s"Engine workload failed: ${cause.message}")
