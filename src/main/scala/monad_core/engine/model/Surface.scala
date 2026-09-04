package monad_core.engine.model

import monad_core.engine.model.Locatable.validateAndReturn

/**
 * Immutable environmental element placed in a [[Scene]].
 *
 * A surface can affect contained entities through friction, an applied force, and damage over time. Use
 * [[Surface.circle]] or [[Surface.rectangle]] to validate the locatable properties before construction.
 *
 * @param id validated identifier that is unique within a scene
 * @param position position in world coordinates
 * @param shape geometric area occupied by the surface
 * @param rotation rotation in degrees in the inclusive range `[0, 360]`
 * @param frictionIndex optional coefficient used by surface dynamics
 * @param appliedForce optional force applied to contained entities
 * @param damageOverTime optional non-negative damage applied during a physics update
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

/** Factory methods and immutable update operations for [[Surface]]. */
object Surface:

  /**
   * Creates a circular surface after validating its locatable properties.
   *
   * @param id raw non-empty identifier
   * @param position position with non-negative coordinates
   * @param radius strictly positive circle radius
   * @param rotation rotation in degrees in the inclusive range `[0, 360]`
   * @return the validated surface, or the first validation error
   */
  def circle(
      id: String,
      position: Vector2D,
      radius: Double,
      rotation: Double = 0
  ): Either[EngineError, Surface] =
    Locatable.circle(id, position, radius, rotation)((id, position, shape, rotation) =>
      Surface(id, position, shape, rotation)
    )

  /**
   * Creates a rectangular surface after validating its locatable properties.
   *
   * @param id raw non-empty identifier
   * @param position position with non-negative coordinates
   * @param height strictly positive rectangle height
   * @param length strictly positive rectangle length
   * @param rotation rotation in degrees in the inclusive range `[0, 360]`
   * @return the validated surface, or the first validation error
   */
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

    /**
     * Replaces or removes the friction index and revalidates the locatable properties.
     *
     * @param frictionIndex replacement friction index, or `None` to remove it
     * @return the updated surface, or a position or rotation validation error
     */
    def withFrictionIndex(frictionIndex: Option[Double]): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(frictionIndex = frictionIndex))

    /**
     * Replaces or removes the applied force and revalidates the locatable properties.
     *
     * @param appliedForce replacement force vector, or `None` to remove it
     * @return the updated surface, or a position or rotation validation error
     */
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
