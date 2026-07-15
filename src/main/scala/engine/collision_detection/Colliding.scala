package engine.collision_detection

import engine.geometry.Collision
import engine.model.Locatable

object Colliding:

  extension (locatable: Locatable)

    infix def hasCollisionWith(other: Locatable)(using detector: CollisionDetector): Option[Collision] =
      detector.collision(locatable, other)
