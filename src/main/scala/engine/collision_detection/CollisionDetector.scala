package engine.collision_detection

import engine.geometry.Placed.placed
import engine.geometry.{Collides, Collision, Contains}
import engine.model.{Locatable, Shape2D}

trait CollisionDetector:

  def collision(first: Locatable, second: Locatable): Option[Collision]

  def isInside(target: Locatable, container: Locatable): Boolean

object CollisionDetector:

  given fromGeometry(using
                     collidesInstance: Collides[Shape2D, Shape2D],
                     containsInstance: Contains[Shape2D]
                    ): CollisionDetector with

    override def collision(first: Locatable, second: Locatable): Option[Collision] =
      collidesInstance.checkCollision(first.placed, second.placed)

    override def isInside(target: Locatable, container: Locatable): Boolean =
      containsInstance.checkIfContains(container.placed, target.position)
