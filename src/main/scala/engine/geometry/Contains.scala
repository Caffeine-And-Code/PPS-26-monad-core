package engine.geometry

import engine.model.Vector2D

trait Contains[A]:

  def contains(container: Placed[A], target: Vector2D): Boolean

object Contains:

  extension [A](container: Placed[A])

    infix def contains(target: Vector2D)(using containsInstance: Contains[A]): Boolean =
      containsInstance.contains(container, target)
