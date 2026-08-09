package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Entity, LocatableId, Vector2D}
import PathRectangle.*
import PathCircle.*

object VertexFinder :
  private val CircleVertexes: Int = 16
  
  def apply(entities: List[Entity]): Map[LocatableId, List[Vector2D]] = {
    entities.map { entity =>
      entity.id -> findVertexesForEntity(entity)
    }.toMap
  }

  private def findVertexesForEntity(entity: Entity): List[Vector2D] =
    entity.shape match
      case Circle(_) =>
        entity.shape.asInstanceOf[Circle].vertexes(entity.position, CircleVertexes)
      case Rectangle(_, _) =>
        entity.shape.asInstanceOf[Rectangle].vertexes(entity.position)