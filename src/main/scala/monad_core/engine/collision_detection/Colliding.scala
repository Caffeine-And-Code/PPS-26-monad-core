package monad_core.engine.collision_detection

import monad_core.engine.geometry.Collision
import monad_core.engine.model.Locatable

/** Provides collision check method on [[monad_core.engine.model.Locatable]] values. */
object Colliding:

  extension (locatable: Locatable)

    /**
     * Checks whether this locatable collides with another locatable.
     *
     * @param other second locatable
     * @param detector collision detector used to perform the query
     * @return collision geometry when the two elements overlap or touch; `None` when they are separated
     */
    infix def hasCollisionWith(other: Locatable)(using
        detector: CollisionDetector
    ): Option[Collision] =
      detector.collision(locatable, other)
