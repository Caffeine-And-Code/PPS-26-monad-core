package monad_core.performance.domain

/** Validated number of entities used as a performance workload size. */
opaque type EntityCount = Int

/** Factory methods for [[EntityCount]]. */
object EntityCount:

  /**
   * Validates and creates an entity count.
   *
   * @param value
   *   candidate entity count
   * @return
   *   the validated count when `value` is positive, otherwise [[InvalidEntityCount]]
   */
  def from(value: Int): Either[PerformanceError, EntityCount] =
    Either.cond(value > 0, value, InvalidEntityCount(value))

  extension (entityCount: EntityCount)
    /** Returns the validated primitive value. */
    def value: Int = entityCount
