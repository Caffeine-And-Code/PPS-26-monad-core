package monad_core.performance.model

/**
 * Strategy used to vary the entity count during a performance experiment.
 *
 * Load measures the expected count, Stress searches for the frame-budget breakpoint, Spike
 * introduces a sudden increase and recovery, and Scalability measures the complete growth.
 */
enum PerformanceKind:
  case Load, Stress, Spike, Scalability

/**
 * Entity-count plan required by one performance strategy.
 *
 * Each case contains only the values used by that strategy.
 */
enum PerformancePlan:
  /** Measures one expected entity count. */
  case Load(entities: EntityCount)

  /** Measures the initial load, a sudden maximum load and the initial load again. */
  case Spike(range: EntityRange)

  /** Increases the load until the first frame-budget breakpoint. */
  case Stress(growth: EntityGrowth)

  /** Measures every entity count in a complete growth. */
  case Scalability(growth: EntityGrowth)

  /**
   * Returns the strategy represented by this plan.
   *
   * @return
   *   corresponding performance strategy
   */
  def kind: PerformanceKind = this match
    case Load(_)        => PerformanceKind.Load
    case Stress(_)      => PerformanceKind.Stress
    case Spike(_)       => PerformanceKind.Spike
    case Scalability(_) => PerformanceKind.Scalability

object PerformancePlan:
  /** Default initial number of entities. */
  val DefaultStartEntities = 100

  /** Default maximum number of entities. */
  val DefaultMaximumEntities = 1_600

  /** Default multiplier between consecutive entity counts. */
  val DefaultGrowthFactor = 2

  /**
   * Creates a load plan.
   *
   * @param entities
   *   entity count to measure
   * @return
   *   the validated plan, or an invalid entity-count error
   * @see
   *   [[monad_core.performance.model.EntityCount EntityCount]]
   */
  def load(entities: Int): Either[PerformanceError, PerformancePlan] =
    EntityCount.from(entities).map(Load.apply)

  /**
   * Creates a spike plan.
   *
   * @param start
   *   entity count measured before and after the spike
   * @param maximum
   *   entity count measured during the spike
   * @return
   *   the validated plan, or the first invalid argument
   * @see
   *   [[monad_core.performance.model.EntityRange EntityRange]]
   */
  def spike(start: Int, maximum: Int): Either[PerformanceError, PerformancePlan] =
    EntityRange.from(start, maximum).map(Spike.apply)

  /**
   * Creates a stress plan.
   *
   * @param start
   *   first entity count
   * @param maximum
   *   greatest entity count
   * @param factor
   *   multiplier applied between consecutive counts
   * @return
   *   the validated plan, or the first invalid argument
   * @see
   *   [[monad_core.performance.model.EntityGrowth EntityGrowth]]
   */
  def stress(
      start: Int,
      maximum: Int,
      factor: Int
  ): Either[PerformanceError, PerformancePlan] =
    EntityGrowth.from(start, maximum, factor).map(Stress.apply)

  /**
   * Creates a scalability plan.
   *
   * @param start
   *   first entity count
   * @param maximum
   *   greatest entity count
   * @param factor
   *   multiplier applied between consecutive counts
   * @return
   *   the validated plan, or the first invalid argument
   * @see
   *   [[monad_core.performance.model.EntityGrowth EntityGrowth]]
   */
  def scalability(
      start: Int,
      maximum: Int,
      factor: Int
  ): Either[PerformanceError, PerformancePlan] =
    EntityGrowth.from(start, maximum, factor).map(Scalability.apply)
