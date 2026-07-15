package engine.geometry

trait Collides[A, B]:

  def collision(first: Placed[A], second: Placed[B]): Option[Collision]