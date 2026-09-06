package monad_core.engine.collision_detection

import monad_core.engine.model.Locatable

/** Provides containment method on [[monad_core.engine.model.Locatable]] values. */
object Containing:

  extension (locatable: Locatable)

    /**
     * Checks whether this locatable's position is inside another locatable's shape.
     *
     * @param container locatable used as container
     * @param detector collision detector used to perform the containment query
     * @return `true` when this locatable's position is inside or on the boundary of `container`
     */
    infix def isInside(container: Locatable)(using detector: CollisionDetector): Boolean =
      detector.isInside(locatable, container)
