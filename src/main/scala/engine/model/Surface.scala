package engine.model

case class Surface(
                    id: String,
                    position: Vector2D,
                    shape: Shape2D,
                    frictionIndex: Double = 0,
                    appliedForce: Vector2D = Vector2D(0, 0)
                  ) extends Locatable

object Surface:
  def circle(id: String, position: Vector2D, radius: Double):Either[String, Surface] =
    Locatable.circle(id, position, radius)((id, position, shape) => Surface(id, position, shape))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double):Either[String, Surface] =
    Locatable.rectangle(id, position, height, length)((id, position, shape) => Surface(id, position, shape))

  def validate(entity: Surface): Either[String, Unit] =
    if entity.frictionIndex < 0 then
      Left("surface cannot have friction index less than 0")
    else
      for {
        result <- Locatable.validate(entity.id, entity.position)
      } yield result

  private def validateAndReturn(updated: Surface): Either[String, Surface] =
    Surface.validate(updated).map(_ => updated)

  extension (surface: Surface)

    def withFrictionIndex(frictionIndex: Double): Either[String, Surface] =
      validateAndReturn(surface.copy(frictionIndex = frictionIndex))

    def withAppliedForce(appliedForce: Vector2D): Either[String, Surface] =
      validateAndReturn(surface.copy(appliedForce = appliedForce))
