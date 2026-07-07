package engine.collision_detection

import engine.geometry.Contains
import engine.geometry.Contains.contains
import engine.geometry.Placed.placed
import engine.model.{Locatable, Shape2D}

object Containing:

  extension (locatable: Locatable)
    infix def isInside(container: Locatable)(using Contains[Shape2D]): Boolean =
      container.placed contains locatable.position
