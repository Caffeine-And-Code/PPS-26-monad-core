package engine.model

enum Shape2D:
  case Circle(radius: Double)

object Shape2D :
  def circle(radius: Double): Shape2D =
    Shape2D.Circle(radius)
