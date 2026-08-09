package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Entity
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import PathCircle.*
import PathRectangle.*

private[physics] object SizeHelper :

  def verticalShapeSize(entity: Entity): Double =
    entity.shape match
      case Circle(_) => entity.shape.asInstanceOf[Circle].verticalSize()
      case Rectangle(_, _) => entity.shape.asInstanceOf[Rectangle].verticalSize(entity.position)

  def horizontalShapeSize(entity: Entity): Double =
    entity.shape match
      case Circle(_) => entity.shape.asInstanceOf[Circle].horizontalSize()
      case Rectangle(_, _) => entity.shape.asInstanceOf[Rectangle].horizontalSize(entity.position)

