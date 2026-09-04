package monad_core.engine.collision_detection

import monad_core.engine.geometry.Placed.placed
import monad_core.engine.geometry.{Collides, Collision, Contains}
import monad_core.engine.model.{Locatable, Shape2D}

/**
 * Detects collisions and point containment between engine model elements.
 */
trait CollisionDetector:

  /**
   * Checks whether two locatable elements overlap or touch.
   *
   * @param first first element
   * @param second second element
   * @return collision data, or `None` when separated
   */
  def collision(first: Locatable, second: Locatable): Option[Collision]

  /**
   * Checks whether the target position is inside the container shape.
   *
   * @param target element whose position is tested
   * @param container element whose placed shape is the containing region
   * @return `true` when `target.position` is inside or on the boundary of `container`
   */
  def isInside(target: Locatable, container: Locatable): Boolean

/** Provides the default [[CollisionDetector]] implementation. */
object CollisionDetector:

  /**
   * Builds a detector from collision and containment type-class instances for
   * [[monad_core.engine.model.Shape2D]].
   *
   * @param collidesInstance shape collision implementation
   * @param containsInstance shape containment implementation
   * @return detector that delegates model queries to the supplied implementations
   */
  given fromGeometry(using
      collidesInstance: Collides[Shape2D, Shape2D],
      containsInstance: Contains[Shape2D]
  ): CollisionDetector with

    override def collision(first: Locatable, second: Locatable): Option[Collision] =
      collidesInstance.checkCollision(first.placed, second.placed)

    override def isInside(target: Locatable, container: Locatable): Boolean =
      containsInstance.checkIfContains(container.placed, target.position)
