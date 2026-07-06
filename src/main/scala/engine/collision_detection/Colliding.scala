package engine.collision_detection

import engine.geometry.{Collides, Collision, Placed}
import engine.model.{Locatable, Shape2D}
import engine.geometry.Collides.hasCollisionWithPlaced

object Colliding:

  extension (locatable: Locatable)

    infix def hasCollisionWith(other: Locatable)(using Collides[Shape2D, Shape2D]): Option[Collision] = 
      Placed(locatable.position, locatable.shape).hasCollisionWithPlaced(Placed(other.position, other.shape))
    
