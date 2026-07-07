package engine.collision_detection

import engine.geometry.Placed.placed
import engine.geometry.{Collides, Collision}
import engine.model.{Locatable, Shape2D}

object Colliding:

  extension (locatable: Locatable)

    infix def hasCollisionWith(other: Locatable)(using collisionInstance: Collides[Shape2D, Shape2D]): Option[Collision] =
      collisionInstance.collision(locatable.placed, other.placed)
