package engine.model

case class Surface(
                    id: String,
                    position: Vector2D,
                    shape: Shape2D,
                    frictionIndex: Double = 0
                  ) extends Locatable

object Surface:
  def circle(id: String, position: Vector2D, radius: Double):Either[String, Surface] =
    Locatable.circle(id, position, radius)((id, position, shape) => Surface(id, position, shape))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double):Either[String, Surface] =
    Locatable.rectangle(id, position, height, length)((id, position, shape) => Surface(id, position, shape))