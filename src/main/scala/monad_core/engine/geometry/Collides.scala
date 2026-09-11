package monad_core.engine.geometry

/**
 * Computes collision information between two kinds of placed shapes.
 *
 * @tparam A type of the first shape
 * @tparam B type of the second shape
 */
trait Collides[A, B]:

  /**
   * Checks whether two placed shapes overlap or touch.
   *
   * The returned normal is oriented from `first` to `second`.
   *
   * @param first first placed item
   * @param second second placed item
   * @return collision geometry when the shapes overlap or touch; `None` when they are not colliding
   */
  def checkCollision(first: Placed[A], second: Placed[B]): Option[Collision]
