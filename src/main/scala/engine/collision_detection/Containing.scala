package engine.collision_detection

import engine.geometry.Contains.contains
import engine.geometry.Contains
import engine.geometry.Placed
import engine.model.{Locatable, Shape2D, Vector2D}

import scala.annotation.targetName

object Containing:

  extension (locatable: Locatable)
    infix def isInside(container: Locatable)(using Contains[Shape2D]): Boolean =
      Placed(container.position, container.shape) contains locatable.position
