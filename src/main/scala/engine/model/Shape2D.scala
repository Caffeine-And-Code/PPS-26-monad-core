package engine.model

enum Shape2D:
  case Circle(radius: Double) extends Shape2D
  case Rectangle(height: Double, length: Double) extends Shape2D

object Shape2D :
  def circle(radius: Double): Either[String, Shape2D] =
    if radius <= 0 then
      Left("Radius must be greater than 0")
    else
      Right(Shape2D.Circle(radius))

  def rectangle(height: Double, length: Double): Either[String, Shape2D] = {
    if height <= 0 then
      Left("Height must be greater than 0")
    else if length <= 0 then
      Left("Length must be greater than 0")
    else
      Right(Shape2D.Rectangle(height, length))
  }
