package monad_core.engine.model

import monad_core.engine.model.Locatable.validateAndReturn

/**
 * Immutable environmental element placed in a [[Scene]].
 *
 * A surface can optionally affect entities through friction and an applied
 * force. Use the factory methods to create validated instances.
 */
final case class Surface private (
    id: LocatableId,
    position: Vector2D,
    shape: Shape2D,
    rotation: Double,
    frictionIndex: Option[Double] = None,
    appliedForce: Option[Vector2D] = None,
    damageOverTime: Option[Damage] = None
) extends Locatable

object Surface:

  /** Creates a validated circular surface. */
  def circle(
      id: String,
      position: Vector2D,
      radius: Double,
      rotation: Double = 0
  ): Either[EngineError, Surface] =
    Locatable.circle(id, position, radius, rotation)((id, position, shape, rotation) =>
      Surface(id, position, shape, rotation)
    )

  /** Creates a validated rectangular surface. */
  def rectangle(
      id: String,
      position: Vector2D,
      height: Double,
      length: Double,
      rotation: Double = 0
  ): Either[EngineError, Surface] =
    Locatable.rectangle(id, position, height, length, rotation)((id, position, shape, rotation) =>
      Surface(id, position, shape, rotation)
    )

  extension (surface: Surface)

    /** Returns a copy with the given friction index. */
    def withFrictionIndex(frictionIndex: Option[Double]): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(frictionIndex = frictionIndex))

    /** Returns a copy that applies the given force vector. */
    def withAppliedForce(appliedForce: Option[Vector2D]): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(appliedForce = appliedForce))

    /**
     * Returns a copy with validated damage applied to entities contained by this surface.
     *
     * @param damageOverTime
     * non-negative damage applied during a physics update
     * @return
     * the updated surface, or `DamageCannotBeNegative` for a negative value
     */
    def withDamageOverTime(damageOverTime: Option[Int]): Either[EngineError, Surface] =
      Damage.fromOption(damageOverTime).map(value => surface.copy(damageOverTime = value))
