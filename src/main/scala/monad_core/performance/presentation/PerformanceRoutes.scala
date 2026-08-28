package monad_core.performance.presentation

/** Command names that route the launcher to each performance experiment. */
object PerformanceRoutes:

  /** Command that runs a load experiment. */
  val Load = "performance-load-test"

  /** Command that runs a progressively increasing stress experiment. */
  val Stress = "performance-stress-test"

  /** Command that runs a sudden maximum-load spike experiment. */
  val Spike = "performance-spike-test"

  /** Command that runs a full workload-growth scalability experiment. */
  val Scalability = "performance-scalability-test"
