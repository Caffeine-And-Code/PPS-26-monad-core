package engine.collision_detection

import engine.geometry.{Collides, Collision, Placed}
import engine.model.{Locatable, Shape2D}

object Colliding:

  extension (locatable: Locatable)

    infix def hasCollisionWith(other: Locatable)(using collisionInstance: Collides[Shape2D, Shape2D]): Option[Collision] =
      collisionInstance.collision(Placed(locatable.position, locatable.shape), Placed(other.position, other.shape))
