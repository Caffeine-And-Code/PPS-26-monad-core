package monad_core.simulator.application.performance

import monad_core.engine.simulator.EngineFacade.PhysicsRuleStatus
import monad_core.simulator.domain.performance.{
  PerformanceConfig,
  PerformanceError,
  PerformancePlan
}

/** Application port for executing and rendering performance experiments. */
trait PerformanceExecutor:

  /** 
    * Runs a performance command using the default physics configuration.
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
  ): Either[PerformanceError, String]

  /** 
    * Runs a performance command using the supplied physics-rule configuration.
    * 
    * @param route
    *   selected performance route
    * @param arguments
    *   command-line arguments
    * @param rules
    *   enabled state of the runtime's configurable physics rules
    * @return
    *   the formatted report, or the first validation or engine error
    */
  def runWithRules(
      route: String,
      arguments: Array[String],
      rules: Vector[PhysicsRuleStatus]
  ): Either[PerformanceError, String]

object PerformanceExecutor:
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

  /** Default initial entity count exposed to performance clients. */
  val DefaultStartEntities: Int = PerformancePlan.DefaultStartEntities

  /** Default maximum entity count exposed to performance clients. */
  val DefaultMaximumEntities: Int = PerformancePlan.DefaultMaximumEntities

  /** Default entity growth factor exposed to performance clients. */
  val DefaultGrowthFactor: Int = PerformancePlan.DefaultGrowthFactor

  /** Default measured iteration count exposed to performance clients. */
  val DefaultIterations: Int = PerformanceConfig.DefaultIterations

  /** Default warm-up count exposed to performance clients. */
  val DefaultWarmups: Int = PerformanceConfig.DefaultWarmups

  /** Default frame budget in milliseconds exposed to performance clients. */
  val DefaultFrameBudgetMillis: Long = PerformanceConfig.DefaultFrameBudgetMillis
