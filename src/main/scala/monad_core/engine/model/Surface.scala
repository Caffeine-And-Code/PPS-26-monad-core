package monad_core.engine.model

import monad_core.engine.model.Locatable.validateAndReturn

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

  def circle(
      id: String,
      position: Vector2D,
      radius: Double,
      rotation: Double = 0
  ): Either[EngineError, Surface] =
    Locatable.circle(id, position, radius, rotation)((id, position, shape, rotation) =>
      Surface(id, position, shape, rotation)
    )

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

    def withFrictionIndex(frictionIndex: Double): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(frictionIndex = Some(frictionIndex)))

    def withAppliedForce(appliedForce: Vector2D): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(appliedForce = Some(appliedForce)))

    def withDamageOverTime(damage: Int): Either[EngineError, Surface] =
      Damage(damage).map(value => surface.copy(damageOverTime = Some(value)))
