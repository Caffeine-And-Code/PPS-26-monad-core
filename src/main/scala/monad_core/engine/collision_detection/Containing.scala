package monad_core.engine.collision_detection

import monad_core.engine.model.Locatable

object Containing:

  extension (locatable: Locatable)
    infix def isInside(container: Locatable)(using detector: CollisionDetector): Boolean =
      detector.isInside(locatable, container)
