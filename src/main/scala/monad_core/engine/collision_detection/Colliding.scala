package monad_core.engine.collision_detection

import monad_core.engine.geometry.Collision
import monad_core.engine.model.Locatable

object Colliding:

  extension (locatable: Locatable)

    infix def hasCollisionWith(other: Locatable)(using
        detector: CollisionDetector
    ): Option[Collision] =
      detector.collision(locatable, other)
