package engine.model

case class Vector2D(x: Double, y: Double)

extension (v: Vector2D)
  infix def add(toAdd: Vector2D): Vector2D =
    Vector2D(v.x + toAdd.x, v.y + toAdd.y)

  infix def times(scalar: Double): Vector2D =
    Vector2D(v.x * scalar, v.y * scalar)

  def +(toAdd: Vector2D): Vector2D = v add toAdd

  def *(scalar: Double): Vector2D = v times scalar