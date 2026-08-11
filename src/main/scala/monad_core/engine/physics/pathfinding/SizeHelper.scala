package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Entity
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.physics.pathfinding.PathCircle.*
import monad_core.engine.physics.pathfinding.PathRectangle.*

private[physics] object SizeHelper :

  def verticalShapeSize(entity: Entity): Double =
    entity.shape match
      case circle: Circle => circle.verticalSize()
      case rectangle: Rectangle => rectangle.verticalSize(entity.position)

  def horizontalShapeSize(entity: Entity): Double =
    entity.shape match
      case circle: Circle => circle.horizontalSize()
      case rectangle: Rectangle => rectangle.horizontalSize(entity.position)

