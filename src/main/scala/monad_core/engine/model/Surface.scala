package monad_core.engine.model

import monad_core.engine.errors.EngineError

final case class Surface private(
                                  id: LocatableId,
                                  position: Vector2D,
                                  shape: Shape2D,
                                  rotation: Double,
                                  frictionIndex: Option[Double] = None,
                                  appliedForce: Option[Vector2D] = None
                                ) extends Locatable

object Surface:
  def circle(id: String, position: Vector2D, radius: Double, rotation: Double = 0): Either[EngineError, Surface] =
    Locatable.circle(id, position, radius, rotation)((id, position, shape, rotation) => Surface(id, position, shape, rotation))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double, rotation: Double = 0): Either[EngineError, Surface] =
    Locatable.rectangle(id, position, height, length, rotation)((id, position, shape, rotation) => Surface(id, position, shape, rotation))

  private def validateAndReturn(updated: Surface): Either[EngineError, Surface] = {
    for {
      _ <- Locatable.validatePosition(updated.position)
      _ <- Locatable.validateRotation(updated.rotation)
    } yield updated
  }

  extension (surface: Surface)

    def withFrictionIndex(frictionIndex: Double): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(frictionIndex = Some(frictionIndex)))

    def withAppliedForce(appliedForce: Vector2D): Either[EngineError, Surface] =
      validateAndReturn(surface.copy(appliedForce = Some(appliedForce)))
