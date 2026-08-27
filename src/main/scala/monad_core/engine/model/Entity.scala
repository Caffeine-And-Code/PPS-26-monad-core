package monad_core.engine.model

/**
 * Interactive element placed in a [[Scene]].
 *
 * Use the factory methods to create validated
 * instances.
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

object Entity:

  /** Creates a validated circular entity. */
  def circle(
      id: String,
      position: Vector2D,
      radius: Double,
      rotation: Double = 0
  ): Either[EngineError, Entity] =
    Locatable.circle(id, position, radius, rotation)((id, position, shape, rotation) =>
      Entity(id, position, shape, rotation)
    )

  /** Creates a validated rectangular entity. */
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

    /** Returns a copy positioned at `newPosition`. */
    def moveTo(newPosition: Vector2D): Entity =
      entity.copy(position = newPosition)

    /** Returns a copy translated by `space`. */
    def moveBy(space: Vector2D): Entity =
      entity.copy(position = entity.position + space)

    /** Returns a copy with a validated rotation. */
    def rotateTo(rotation: Double): Either[EngineError, Entity] =
      for _ <- Locatable.validateRotation(rotation)
      yield entity.copy(rotation = rotation)

    /** Returns a copy with an optional `speed`. */
    def withSpeed(speed: Option[Vector2D]): Entity =
      entity.copy(speed = speed)

    /** Returns a copy with an optional `angularSpeed`. */
    def withAngularSpeed(angularSpeed: Option[Double]): Entity =
      entity.copy(angularSpeed = angularSpeed)

    /** Indicates the entity cannot be moved. */
    def isFixed: Boolean =
      entity.speed.isEmpty && entity.angularSpeed.isEmpty

    /** Returns a copy with an optional strictly positive `weight`. */
    def withWeight(weight: Option[Int]): Either[EngineError, Entity] =
      Weight.fromOption(weight).map(w => entity.copy(weight = w))

    /** Returns a copy with an optional `health` */
    def withHealth(health: Option[Int]): Either[EngineError, Entity] =
      Health.fromOption(health).map(h => entity.copy(health = h))

    /**
     * Returns a copy with validated contact damage.
     *
     * @param damage
     *   non-negative damage assigned to the entity or None
     * @return
     *   the updated entity, or `DamageCannotBeNegative` for a negative value
     */
    def withDamage(damage: Option[Int]): Either[EngineError, Entity] =
      Damage.fromOption(damage).map(d => entity.copy(damage = d))

    /**
     * Applies `damage` to the entity's `health`.
     *
     * @return
     *   the updated entity, or an error if it has no health, damage is
     *   negative, or the remaining health is not positive
     */
    def applyDamage(damage: Int): Either[EngineError, Entity] =
      entity.health match
        case None         => Left(CannotApplyDamageToNoneHealthEntity())
        case Some(health) => (health - damage).map(health => entity.copy(health = Some(health)))

    /** Returns a copy optionally assigned to the validated `teamId`. */
    def withTeamId(teamId: Option[String]): Either[EngineError, Entity] =
      TeamId.fromOption(teamId).map(t => entity.copy(teamId = t))
