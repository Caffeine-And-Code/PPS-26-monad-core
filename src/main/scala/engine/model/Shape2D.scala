package engine.model

import engine.errors.EngineError

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

  extension (rectangle: Rectangle)

    private def halfHeight: Double = rectangle.height / 2

    private def halfLength: Double = rectangle.length / 2

    def getXRange(positionX: Double): (Double, Double) =
      (positionX - rectangle.halfLength, positionX + rectangle.halfLength)

    def getYRange(positionY: Double): (Double, Double) =
      (positionY - rectangle.halfHeight, positionY + rectangle.halfHeight)