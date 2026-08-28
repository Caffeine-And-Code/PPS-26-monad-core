package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Entity, LocatableId, Vector2D}
import monad_core.engine.physics.pathfinding.CircleVertexes.*
import monad_core.engine.physics.pathfinding.RectangleVertexes.*

/** Builds the world-space vertex representation used by pathfinding. */
private[engine] object VertexFinder:
  /** Number of points used to approximate a circle. */
  private val CircleVertexesNumber: Int = 16

  /**
   * Returns the vertices of every supplied entity indexed by its identifier.
   *
   * @param entities
   *  the entities whose vertices are to be computed
   * @return
   *  a map of entity identifiers to their corresponding vertices in world coordinates
   * */
  def apply(entities: List[Entity]): Map[LocatableId, List[Vector2D]] =
    entities.map { entity =>
      entity.id -> findVertexesForEntity(entity)
    }.toMap

  /**
   *  Selects the appropriate vertex calculation for an entity shape.
   *
   * @param entity
   *  the entity whose vertices are to be computed
   * @return
   *  a list of the entity's vertices in world coordinates
   *  */
  private def findVertexesForEntity(entity: Entity): List[Vector2D] =
    entity.shape match
      case circle: Circle =>
        circle.vertexes(entity.position, CircleVertexesNumber)
      case rectangle: Rectangle =>
        rectangle.vertexes(entity.position, entity.rotation)
