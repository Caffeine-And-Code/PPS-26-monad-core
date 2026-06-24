package engine.model

enum Shape2D:
  case Circle(radius: Double) extends Shape2D
  case Rectangle(height: Double, length: Double) extends Shape2D

object Shape2D :
  def circle(radius: Double): Shape2D = {
    require(radius > 0, "radius must be greater than 0")
    Shape2D.Circle(radius)
  }

  def rectangle(height: Double, length: Double) =
    Shape2D.Rectangle(height, length)
