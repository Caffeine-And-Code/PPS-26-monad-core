package engine.collision_detection

import engine.model.Shape2D.{Circle, Rectangle}
import engine.model.*

object Containing {

  extension (position: Vector2D)

    private def isInsideCircle(otherPosition: Vector2D, circle: Circle): Boolean =
      (position --> otherPosition) <= circle.radius

  extension (locatable: Locatable)

    infix def isInside(other: Locatable): Boolean =
      other.shape match
        case circle: Circle => locatable.position.isInsideCircle(other.position, circle)
        case rectangle: Rectangle => ???
}

