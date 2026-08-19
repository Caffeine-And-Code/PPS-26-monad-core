package monad_core.simulator.application.engine

import scalafx.scene.paint.Color

enum DrawCommand:
  case Circle(x: Double, y: Double, radius: Double, color: Color)
  case Rectangle(x: Double, y: Double, width: Double, height: Double, color: Color)

trait ShapeArchitect:
  def drainBuffer(): List[DrawCommand]
