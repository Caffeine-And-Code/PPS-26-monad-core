package monad_core.engine.model

import monad_core.engine.errors.EngineError

final case class Surface private(
                                  id: LocatableId,
                                  position: Vector2D,
                                  shape: Shape2D,
                                  frictionIndex: Option[Double] = None,
                                  appliedForce: Option[Vector2D] = None
                                ) extends Locatable

object Surface:
  def circle(id: String, position: Vector2D, radius: Double): Either[EngineError, Surface] =
    Locatable.circle(id, position, radius)((id, position, shape) => Surface(id, position, shape))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double): Either[EngineError, Surface] =
    Locatable.rectangle(id, position, height, length)((id, position, shape) => Surface(id, position, shape))

  private def validateAndReturn(updated: Surface): Either[EngineError, Surface] =
    Locatable.validate(updated.position).map(_ => updated)

  extension (surface: Surface)

    def withFrictionIndex(frictionIndex: Double): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(frictionIndex = Some(frictionIndex)))

    def withAppliedForce(appliedForce: Vector2D): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(appliedForce = Some(appliedForce)))
