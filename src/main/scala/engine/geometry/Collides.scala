package engine.geometry

trait Collides[A, B]:

  def collision(first: Placed[A], second: Placed[B]): Option[Collision]

object Collides:

  extension [A](first: Placed[A])

    infix def hasCollisionWithPlaced[B](second: Placed[B])(using collidesInstance: Collides[A, B]): Option[Collision] =
      collidesInstance.collision(first, second)