package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Entity
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.physics.pathfinding.CircleVertexes.*
import monad_core.engine.physics.pathfinding.RectangleVertexes.*

/** Computes axis-aligned entity dimensions for all supported shapes. */
private[physics] object SizeHelper:

  /**
   * Returns the vertical size of an entity after applying its rotation.
   *
   * @param entity
   *  the entity whose vertical size is to be computed
   * @return
   *  the vertical size of the entity after applying its rotation
   * */
  def verticalShapeSize(entity: Entity): Double =
    entity.shape match
      case circle: Circle       => circle.verticalSize()
      case rectangle: Rectangle => rectangle.verticalSize(entity.position, entity.rotation)

  /**
   * Returns the horizontal size of an entity after applying its rotation.
   *
   * @param entity
   *  the entity whose horizontal size is to be computed
   * @return
   *  the horizontal size of the entity after applying its rotation
   * */
  def horizontalShapeSize(entity: Entity): Double =
    entity.shape match
      case circle: Circle       => circle.horizontalSize()
      case rectangle: Rectangle => rectangle.horizontalSize(entity.position, entity.rotation)
