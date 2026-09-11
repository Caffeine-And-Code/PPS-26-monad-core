package monad_core.engine.simulator

import monad_core.engine.model.EngineColor

/** Backend-independent instruction describing a primitive to draw. */
enum DrawCommand:

  /**
   * Draws a circle centered at the supplied coordinates.
   *
   * @param x
   *   horizontal center coordinate
   * @param y
   *   vertical center coordinate
   * @param radius
   *   circle radius
   * @param color
   *   fill color
   */
  case Circle(x: Double, y: Double, radius: Double, color: EngineColor)

  /**
   * Draws a rectangle centered at the supplied coordinates and rotated around its center.
   *
   * @param x
   *   horizontal center coordinate
   * @param y
   *   vertical center coordinate
   * @param width
   *   rectangle width
   * @param height
   *   rectangle height
   * @param rotation
   *   clockwise rotation in degrees
   * @param color
   *   fill color
   */
  case Rectangle(
      x: Double,
      y: Double,
      width: Double,
      height: Double,
      rotation: Double,
      color: EngineColor
  )
