package monad_core.simulator.presentation.painters

import monad_core.engine.model.EngineColor
import monad_core.engine.model.EngineColor.{HSL, RGB}
import monad_core.engine.simulator.DrawCommand
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

/** ScalaFX interpreter for backend-independent engine drawing commands. */
object ShapePainter:

  extension (engineColor: EngineColor)

    private def toColor: Color =
      engineColor match
        case RGB(r, g, b) => Color.rgb(r.value, g.value, b.value)
        case HSL(h, s, l) => Color.hsb(h.value, s.value / 100.0, l.value / 100.0)

  /**
   * Clears a canvas and executes drawing commands in their original order.
   *
   * Circles are positioned by their center. Rectangles are translated to their center and rotated before being
   * filled, with the graphics context restored after each command.
   *
   * @param gc
   *   target ScalaFX graphics context
   * @param commands
   *   immutable drawing plan to interpret
   */
  def paint(gc: GraphicsContext, commands: Vector[DrawCommand]): Unit =
    gc.clearRect(0, 0, gc.canvas.getWidth, gc.canvas.getHeight)

    commands.foreach:
      case DrawCommand.Circle(x, y, r, c) =>
        gc.fill = c.toColor
        gc.fillOval(x - r, y - r, r * 2, r * 2)
      case DrawCommand.Rectangle(x, y, w, h, rotation, c) =>
        gc.fill = c.toColor
        gc.save()
        gc.translate(x, y)
        gc.rotate(rotation)
        gc.fillRect(-w / 2, -h / 2, w, h)
        gc.restore()
