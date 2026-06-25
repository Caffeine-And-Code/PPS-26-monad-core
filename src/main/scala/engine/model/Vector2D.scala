package engine.model

case class Vector2D(x: Double, y: Double)

extension (v: Vector2D)
  infix def add(toAdd: Vector2D): Vector2D =
    Vector2D(v.x + toAdd.x, v.y + toAdd.y)

  def +(toAdd: Vector2D): Vector2D = v add toAdd