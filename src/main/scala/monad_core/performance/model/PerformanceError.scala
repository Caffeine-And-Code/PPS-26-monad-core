package monad_core.performance.model

import monad_core.engine.model.EngineError
import monad_core.simulator.errors.BaseError

import scala.concurrent.duration.FiniteDuration

sealed abstract class PerformanceError(message: String) extends BaseError(message)

final case class InvalidPositiveCount(name: String, value: Int)
  extends PerformanceError(s"$name must be positive: $value")

final case class InvalidWarmupCount(value: Int)
  extends PerformanceError(s"Warm-up count cannot be negative: $value")

final case class InvalidGrowthFactor(value: Int)
  extends PerformanceError(s"Growth factor must be greater than one: $value")

final case class InvalidGrowthMaximum(start: Int, maximum: Int)
  extends PerformanceError(s"Maximum entity count $maximum cannot be lower than start $start")

final case class InvalidFrameBudget(value: Long)
  extends PerformanceError(s"Frame budget must be positive: $value")

final case class InvalidPerformanceArgument(argument: String, value: String)
  extends PerformanceError(s"Invalid value '$value' for argument '$argument'")

final case class UnknownPerformanceRoute(route: String)
  extends PerformanceError(s"Unknown performance route: $route")

final case class EmptyPerformanceSamples()
  extends PerformanceError("At least one performance sample is required")

final case class EnginePerformanceError(cause: EngineError)
  extends PerformanceError(s"Engine workload failed: ${cause.message}")
