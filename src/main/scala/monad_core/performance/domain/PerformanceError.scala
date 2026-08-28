package monad_core.performance.domain

import monad_core.engine.model.EngineError
import monad_core.simulator.errors.BaseError

/**
 * Base type for failures in the performance feature.
 *
 * @param message
 *   failure description inherited by [[monad_core.simulator.errors.BaseError]]
 */
sealed abstract class PerformanceError(message: String) extends BaseError(message)

/**
 * Indicates that an entity count was zero or negative.
 *
 * @param value
 *   rejected entity count
 */
final case class InvalidEntityCount(value: Int)
    extends PerformanceError(s"Entity count must be positive: $value")

/**
 * Indicates that an iteration count was zero or negative.
 *
 * @param value
 *   rejected iteration count
 */
final case class InvalidIterationCount(value: Int)
    extends PerformanceError(s"Iteration count must be positive: $value")

/**
 * Indicates that a warm-up count was negative.
 *
 * @param value
 *   rejected warm-up count
 */
final case class InvalidWarmupCount(value: Int)
    extends PerformanceError(s"Warm-up count cannot be negative: $value")

/**
 * Indicates that a frame budget was zero or negative.
 *
 * @param value
 *   rejected budget in nanoseconds
 */
final case class InvalidFrameBudget(value: Long)
    extends PerformanceError(s"Frame budget must be positive: $value")

/**
 * Indicates that a growth maximum was lower than its starting entity count.
 *
 * @param start
 *   validated starting entity count
 * @param maximum
 *   rejected maximum entity count
 */
final case class InvalidGrowthMaximum(start: Int, maximum: Int)
    extends PerformanceError(s"Maximum entity count $maximum cannot be lower than start $start")

/**
 * Indicates that an entity-growth factor was not greater than one.
 *
 * @param value
 *   rejected growth factor
 */
final case class InvalidGrowthFactor(value: Int)
    extends PerformanceError(s"Growth factor must be greater than one: $value")

/** Indicates that an aggregate metric was requested without any performance samples. */
final case class EmptyPerformanceSamples()
    extends PerformanceError("At least one performance sample is required")

/**
 * Adapts an engine-layer failure to the performance domain.
 *
 * @param cause
 *   underlying engine error produced during scene construction or ticking
 */
final case class EnginePerformanceError(cause: EngineError)
    extends PerformanceError(s"Engine workload failed: ${cause.message}")

/**
 * Indicates that a command-line performance argument could not be parsed.
 *
 * @param argument
 *   option name associated with the invalid token
 * @param value
 *   rejected command-line token
 */
final case class InvalidPerformanceArgument(argument: String, value: String)
    extends PerformanceError(s"Invalid value '$value' for argument '$argument'")

/**
 * Indicates that no performance experiment is associated with a requested route.
 *
 * @param route
 *   unsupported performance command
 */
final case class UnknownPerformanceRoute(route: String)
    extends PerformanceError(s"Unknown performance route: $route")
