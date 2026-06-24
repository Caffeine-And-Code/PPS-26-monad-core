package engine.model

enum Shape2D:
  case Circle(radius: Double)
  case Rectangle(height: Double, length: Double)

object Shape2D :
  def circle(radius: Double): Shape2D =
    Shape2D.Circle(radius)
    
  def rectangle(height: Double, length: Double) =
    Shape2D.Rectangle(height, length)
