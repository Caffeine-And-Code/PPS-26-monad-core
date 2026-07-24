package monad_core.engine.geometry

trait Collides[A, B]:

  def checkCollision(first: Placed[A], second: Placed[B]): Option[Collision]