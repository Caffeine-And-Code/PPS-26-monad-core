package monad_core.engine.model

/**
 * Adds fluent entity update operations to a construction result.
 *
 * Every operation preserves an existing [[EngineError]]. Validating operations stop the chain at the first invalid
 * property.
 */
object EntityBuilder:

  extension (entity: Either[EngineError, Entity])

    /**
     * Replaces or removes the speed of a successfully constructed entity.
     *
     * @param speed replacement speed, or `None` to remove it
     * @return the updated construction result, preserving an existing error
     */
    def withSpeed(speed: Option[Vector2D]): Either[EngineError, Entity] =
      entity.map(_.withSpeed(speed))

    /**
     * Replaces or removes the angular speed of a successfully constructed entity.
     *
     * @param angularSpeed replacement angular speed, or `None` to remove it
     * @return the updated construction result, preserving an existing error
     */
    def withAngularSpeed(angularSpeed: Option[Double]): Either[EngineError, Entity] =
      entity.map(_.withAngularSpeed(angularSpeed))

    /**
     * Validates and replaces the optional weight of a successfully constructed entity.
     *
     * @param weight strictly positive raw weight, or `None` to remove it
     * @return the updated construction result, a weight validation error, or the existing error
     */
    def withWeight(weight: Option[Int]): Either[EngineError, Entity] =
      entity.flatMap(_.withWeight(weight))

    /**
     * Validates and replaces the optional health of a successfully constructed entity.
     *
     * @param health strictly positive raw health, or `None` to remove it
     * @return the updated construction result, a health validation error, or the existing error
     */
    def withHealth(health: Option[Int]): Either[EngineError, Entity] =
      entity.flatMap(_.withHealth(health))

    /**
     * Validates and replaces the optional damage of a successfully constructed entity.
     *
     * @param damage non-negative raw damage, or `None` to remove it
     * @return the updated construction result, a damage validation error, or the existing error
     */
    def withDamage(damage: Option[Int]): Either[EngineError, Entity] =
      entity.flatMap(_.withDamage(damage))

    /**
     * Validates and replaces the optional team identifier of a successfully constructed entity.
     *
     * @param teamId raw team identifier, or `None` to remove it
     * @return the updated construction result, a team identifier validation error, or the existing error
     */
    def withTeamId(teamId: Option[String]): Either[EngineError, Entity] =
      entity.flatMap(_.withTeamId(teamId))
