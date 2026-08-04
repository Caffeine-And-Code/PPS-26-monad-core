package monad_core.engine.geometry

import monad_core.engine.model.{Locatable, Shape2D, Vector2D}

final case class Placed[A](center: Vector2D, shape: A, rotation: Double = 0.0)

object Placed:
  extension (locatable: Locatable)
    def placed: Placed[Shape2D] =
      Placed(locatable.position, locatable.shape, locatable.rotation)
