package engine.collision_detection

import engine.geometry.Placed.placed
import engine.geometry.{Collides, Collision, Contains}
import engine.model.{Locatable, Shape2D}

trait CollisionDetector:

  def collision(first: Locatable, second: Locatable): Option[Collision]

  def isInside(target: Locatable, container: Locatable): Boolean

object CollisionDetector:

  def apply(using detector: CollisionDetector): CollisionDetector =
    detector

  given fromGeometry(using
                     collides: Collides[Shape2D, Shape2D],
                     contains: Contains[Shape2D]
                    ): CollisionDetector with

    override def collision(first: Locatable, second: Locatable): Option[Collision] =
      collides.collision(first.placed, second.placed)

    override def isInside(target: Locatable, container: Locatable): Boolean =
      contains.contains(container.placed, target.position)
