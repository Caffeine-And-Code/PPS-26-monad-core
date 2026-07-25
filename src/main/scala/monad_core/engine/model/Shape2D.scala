package monad_core.engine.model

import monad_core.engine.errors.EngineError

enum Shape2D:
  case Circle private[model](radius: Double)
  case Rectangle private[model](height: Double, length: Double)

object Shape2D:
  def circle(radius: Double): Either[EngineError, Shape2D] =
    if radius <= 0 then
      Left(RadiusMustBeGreaterThanZero())
    else
      Right(Shape2D.Circle(radius))

  def rectangle(height: Double, length: Double): Either[EngineError, Shape2D] =
    if height <= 0 then
      Left(HeightMustBeGreaterThanZero())
    else if length <= 0 then
      Left(LengthMustBeGreaterThanZero())
    else
      Right(Shape2D.Rectangle(height, length))
