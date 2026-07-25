package monad_core.engine.model

case class Vector2D(x: Double, y: Double)

extension (v: Vector2D)
  infix def add(toAdd: Vector2D): Vector2D =
    Vector2D(v.x + toAdd.x, v.y + toAdd.y)

  infix def sub(toAdd: Vector2D): Vector2D =
    Vector2D(v.x - toAdd.x, v.y - toAdd.y)

  infix def times(scalar: Double): Vector2D =
    Vector2D(v.x * scalar, v.y * scalar)
    
  infix def dot(toDot: Vector2D): Double =
    v.x * toDot.x + v.y * toDot.y

  def +(toAdd: Vector2D): Vector2D = v add toAdd
  
  def -(toSub: Vector2D): Vector2D = v sub toSub

  def *(scalar: Double): Vector2D = v times scalar
  
  def o(toDot: Vector2D): Double = v dot toDot