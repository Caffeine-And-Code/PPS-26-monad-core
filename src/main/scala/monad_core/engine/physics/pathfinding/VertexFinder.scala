package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Entity, LocatableId, Vector2D}
import monad_core.engine.physics.pathfinding.CircleVertexes.*
import monad_core.engine.physics.pathfinding.RectangleVertexes.*

object VertexFinder:
  private val CircleVertexes: Int = 16

  def apply(entities: List[Entity]): Map[LocatableId, List[Vector2D]] =
    entities.map { entity =>
      entity.id -> findVertexesForEntity(entity)
    }.toMap

  private def findVertexesForEntity(entity: Entity): List[Vector2D] =
    entity.shape match
      case circle: Circle =>
        circle.vertexes(entity.position, CircleVertexes)
      case rectangle: Rectangle =>
        rectangle.vertexes(entity.position, entity.rotation)
