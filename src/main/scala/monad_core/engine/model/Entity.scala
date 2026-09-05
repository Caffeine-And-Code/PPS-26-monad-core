package monad_core.engine.model

/**
 * Immutable interactive element placed in a [[Scene]].
 *
 * Use [[Entity.circle]] or [[Entity.rectangle]] to validate and create the locatable properties.
 *
 * @param id validated identifier that is unique within a scene
 * @param position position in world coordinates
 * @param shape geometric area occupied by the entity
 * @param rotation rotation in degrees in the inclusive range `[0, 360]`
 * @param speed optional speed vector
 * @param angularSpeed optional angular speed in degrees per second
 * @param weight optional strictly positive weight
 * @param health optional strictly positive health
 * @param teamId optional identifier of the entity's team
 * @param damage optional non-negative contact damage
 */
final case class Entity private (
    id: LocatableId,
    position: Vector2D,
    shape: Shape2D,
    rotation: Double,
    speed: Option[Vector2D] = None,
    angularSpeed: Option[Double] = None,
    weight: Option[Weight] = None,
    health: Option[Health] = None,
    teamId: Option[TeamId] = None,
    damage: Option[Damage] = None
) extends Locatable

/** Factory methods and immutable update operations for [[Entity]]. */
object Entity:

  /**
   * Creates a circular entity after validating its locatable properties.
   *
   * @param id raw non-empty identifier
   * @param position position with non-negative coordinates
   * @param radius strictly positive circle radius
   * @param rotation rotation in degrees in the inclusive range `[0, 360]`
   * @return the validated entity, or the first validation error
   */
  def circle(
      id: String,
      position: Vector2D,
      radius: Double,
      rotation: Double = 0
  ): Either[EngineError, Entity] =
    Locatable.circle(id, position, radius, rotation)((id, position, shape, rotation) =>
      Entity(id, position, shape, rotation)
    )

  /**
   * Creates a rectangular entity after validating its locatable properties.
   *
   * @param id raw non-empty identifier
   * @param position position with non-negative coordinates
   * @param height strictly positive rectangle height
   * @param length strictly positive rectangle length
   * @param rotation rotation in degrees in the inclusive range `[0, 360]`
   * @return the validated entity, or the first validation error
   */
  def rectangle(
      id: String,
      position: Vector2D,
      height: Double,
      length: Double,
      rotation: Double = 0
  ): Either[EngineError, Entity] =
    Locatable.rectangle(id, position, height, length, rotation)((id, position, shape, rotation) =>
      Entity(id, position, shape, rotation)
    )

  extension (entity: Entity)

    /**
     * Returns a copy positioned at `newPosition`
     *
     * @param newPosition replacement position
     * @return entity copy at the supplied position
     */
    def moveTo(newPosition: Vector2D): Entity =
      entity.copy(position = newPosition)

    /**
     * Translates the entity
     *
     * @param space displacement added component-wise to the current position
     * @return entity copy at the translated position
     */
    def moveBy(space: Vector2D): Entity =
      entity.copy(position = entity.position + space)

    /**
     * Replaces the entity rotation after validation.
     *
     * @param rotation rotation in degrees
     * @return the updated entity when rotation is in `[0, 360]`, or
     *   [[RotationMustBeAValidDegreeValue]] otherwise
     */
    def rotateTo(rotation: Double): Either[EngineError, Entity] =
      for _ <- Locatable.validateRotation(rotation)
      yield entity.copy(rotation = rotation)

    /**
     * Replaces or removes the speed.
     *
     * @param speed replacement speed, or `None` to remove it
     * @return entity copy containing the supplied optional speed
     */
    def withSpeed(speed: Option[Vector2D]): Entity =
      entity.copy(speed = speed)

    /**
     * Replaces or removes the angular speed.
     *
     * @param angularSpeed replacement angular speed, or `None` to remove it
     * @return entity copy containing the supplied optional angular speed
     */
    def withAngularSpeed(angularSpeed: Option[Double]): Entity =
      entity.copy(angularSpeed = angularSpeed)

    /**
     * Returns whether the entity has neither linear nor angular speed.
     *
     * @return `true` when both speed properties are absent
     */
    def isFixed: Boolean =
      entity.speed.isEmpty && entity.angularSpeed.isEmpty

    /**
     * Set the entity weight.
     *
     * @param weight strictly positive raw weight, or `None` to remove it
     * @return the updated entity, or [[WeightCannotBeNegativeOrZero]] for a non-positive value
     */
    def withWeight(weight: Option[Int]): Either[EngineError, Entity] =
      Weight.fromOption(weight).map(w => entity.copy(weight = w))

    /**
     * Set the entity health.
     *
     * @param health strictly positive raw health, or `None` to remove it
     * @return the updated entity, or [[HealthCannotBeNegativeOrZero]] for a non-positive value
     */
    def withHealth(health: Option[Int]): Either[EngineError, Entity] =
      Health.fromOption(health).map(h => entity.copy(health = h))

    /**
     * Returns a copy with validated contact damage.
     *
     * @param damage non-negative raw damage assigned to the entity, or `None` to remove it
     * @return the updated entity, or [[DamageCannotBeNegative]] for a negative value
     */
    def withDamage(damage: Option[Int]): Either[EngineError, Entity] =
      Damage.fromOption(damage).map(d => entity.copy(damage = d))

    /**
     * Applies `damage` to the entity's `health`.
     *
     * @param damage non-negative health points to subtract
     * @return the updated entity, or an error when health is absent, damage is negative, or the remaining health is
     *   non-positive
     */
    def applyDamage(damage: Int): Either[EngineError, Entity] =
      entity.health match
        case None         => Left(CannotApplyDamageToNoneHealthEntity())
        case Some(health) => (health - damage).map(health => entity.copy(health = Some(health)))

    /**
     * Set the entity team identifier.
     *
     * @param teamId raw team identifier, or `None` to remove the assignment
     * @return the updated entity, or [[TeamIdCannotBeEmpty]] for an empty identifier
     */
    def withTeamId(teamId: Option[String]): Either[EngineError, Entity] =
      TeamId.fromOption(teamId).map(t => entity.copy(teamId = t))
