package engine.model

final case class Entity(id: String, position: Vector2D, shape: Shape2D) {
  require(id.trim.nonEmpty, "Entity ID must not be empty")
  require(position.x > 0 && position.y > 0, "position coordinates X and Y must be greater than 0")
}

object Entity:
  def circle(id: String, position: Vector2D, radius: Double):Entity =
    Entity(id, position, Shape2D.circle(radius))

  def rectangle(id: String, position: Vector2D, height: Double, length: Double):Entity =
    Entity(id, position, Shape2D.rectangle(height, length))