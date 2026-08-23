package monad_core.simulator.application.engine

import monad_core.engine.model.EngineColor

enum DrawCommand:
  case Circle(x: Double, y: Double, radius: Double, color: EngineColor)

  case Rectangle(
      x: Double,
      y: Double,
      width: Double,
      height: Double,
      rotation: Double,
      color: EngineColor
  )

trait ShapeArchitect:
  def drainBuffer(): List[DrawCommand]
