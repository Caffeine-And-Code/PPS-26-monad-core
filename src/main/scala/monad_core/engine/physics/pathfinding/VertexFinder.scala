package monad_core.engine.physics.pathfinding

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{Entity, LocatableId, Vector2D}

object VertexFinder {
  private val CircleVertexes: Int = 16
  
  def apply(entities: List[Entity]): Map[LocatableId, List[Vector2D]] =
    entities.map { entity =>
      entity.id -> findVertexesForEntity(entity)
    }.toMap
    
  def verticalShapeSize(entity: Entity): Double =
    entity.shape match
      case Circle(radius) => radius * 2
      case Rectangle(_, _) => 
        val rectangle = entity.shape.asInstanceOf[Rectangle]
        val upperVertex = rectangle.upperVertex(entity.position)
        val lowerVertex = rectangle.lowerVertex(entity.position)
        lowerVertex.y - upperVertex.y

  def horizontalShapeSize(entity: Entity): Double =
    entity.shape match
      case Circle(radius) => radius * 2
      case Rectangle(_, _) => 
        val rectangle = entity.shape.asInstanceOf[Rectangle]
        val leftVertex = rectangle.leftVertex(entity.position)
        val rightVertex = rectangle.rightVertex(entity.position)
        rightVertex.x - leftVertex.x
  
  private def findVertexesForEntity(entity: Entity): List[Vector2D] =
    entity.shape match
      case Circle(radius) =>
        findVertexesForCircle(entity)
      case Rectangle(height, length) =>
        entity.shape.asInstanceOf[Rectangle].vertexes(entity.position)

  private def findVertexesForCircle(entity: Entity): List[Vector2D] =

    val circle = entity.shape.asInstanceOf[Circle]

    (0 until CircleVertexes).map { i =>

      val angle = 2 * math.Pi * i / CircleVertexes

      PointOnCircle(entity.position, circle.radius, angle)

    }.toList
}
