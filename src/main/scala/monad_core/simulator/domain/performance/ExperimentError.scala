package monad_core.simulator.domain.performance

import monad_core.simulator.errors.BaseError

/**
 * Indicates that a required performance form argument was not submitted.
 *
 * @param argument
 *   missing form argument
 */
final case class MissingPerformanceArgument(argument: String)
    extends BaseError(s"Missing performance argument: $argument")

/**
 * Indicates that a graphical performance-test selection is not supported.
 *
 * @param label
 *   rejected selection label
 */
final case class UnknownPerformanceExperimentType(label: String)
    extends BaseError(s"Unknown performance test type: $label")
