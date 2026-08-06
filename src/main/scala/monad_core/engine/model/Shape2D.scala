package monad_core.engine.model

import monad_core.engine.errors.EngineError

enum Shape2D:
  case Circle (radius: Double)
  case Rectangle (height: Double, length: Double)

object Shape2D:
  def circle(radius: Double): Either[EngineError, Circle] =
    if radius <= 0 then
      Left(RadiusMustBeGreaterThanZero())
    else
      Right(Shape2D.Circle(radius))

  def rectangle(height: Double, length: Double): Either[EngineError, Rectangle] =
    if height <= 0 then
      Left(HeightMustBeGreaterThanZero())
    else if length <= 0 then
      Left(LengthMustBeGreaterThanZero())
    else
      Right(Shape2D.Rectangle(height, length))

  extension (rectangle:Rectangle)
    def halfLength: Double = rectangle.length / 2
    def halfHeight: Double = rectangle.height / 2

    def vertexes(position: Vector2D): List[Vector2D] =
      List(
        Vector2D(
          position.x - rectangle.halfLength,
          position.y - rectangle.halfHeight
        ),
        Vector2D(
          position.x + rectangle.halfLength,
          position.y - rectangle.halfHeight
        ),
        Vector2D(
          position.x + rectangle.halfLength,
          position.y + rectangle.halfHeight
        ),
        Vector2D(
          position.x - rectangle.halfLength,
          position.y + rectangle.halfHeight
        )
      )
      
    def upperVertex(position: Vector2D): Vector2D =
      vertexes(position).maxBy(_.y)
      
    def lowerVertex(position: Vector2D): Vector2D =
      vertexes(position).minBy(_.y)
      
    def leftVertex(position: Vector2D): Vector2D =
      vertexes(position).minBy(_.x)
      
    def rightVertex(position: Vector2D): Vector2D =
      vertexes(position).maxBy(_.x)