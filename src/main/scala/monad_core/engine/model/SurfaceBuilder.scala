package monad_core.engine.model

/**
 * Adds fluent surface update operations to a construction result.
 *
 * Every operation preserves an existing [[EngineError]] and stops the chain when validation fails.
 */
object SurfaceBuilder:

  extension (surface: Either[EngineError, Surface])

    /**
     * Replaces or removes the friction index of a successfully constructed surface.
     *
     * @param frictionIndex replacement friction index, or `None` to remove it
     * @return the updated construction result, a locatable validation error, or the existing error
     */
    def withFrictionIndex(frictionIndex: Option[Double]): Either[EngineError, Surface] =
      surface.flatMap(_.withFrictionIndex(frictionIndex))

    /**
     * Replaces or removes the applied force of a successfully constructed surface.
     *
     * @param appliedForce replacement force vector, or `None` to remove it
     * @return the updated construction result, a locatable validation error, or the existing error
     */
    def withAppliedForce(appliedForce: Option[Vector2D]): Either[EngineError, Surface] =
      surface.flatMap(_.withAppliedForce(appliedForce))

    /**
     * Validates and replaces the optional damage over time of a successfully constructed surface.
     *
     * @param damageOverTime non-negative raw damage, or `None` to remove it
     * @return the updated construction result, a damage validation error, or the existing error
     */
    def withDamageOverTime(damageOverTime: Option[Int]): Either[EngineError, Surface] =
      surface.flatMap(_.withDamageOverTime(damageOverTime))
