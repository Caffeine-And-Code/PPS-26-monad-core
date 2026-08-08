package monad_core.engine.geometry

import monad_core.engine.model.Vector2D

trait Contains[A]:

  def checkIfContains(container: Placed[A], target: Vector2D): Boolean

object Contains:

  extension [A](container: Placed[A])

    infix def contains(target: Vector2D)(using containsInstance: Contains[A]): Boolean =
      containsInstance.checkIfContains(container, target)
