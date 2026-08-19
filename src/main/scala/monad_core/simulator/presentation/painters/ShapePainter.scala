package monad_core.simulator.presentation.painters

import monad_core.engine.model.EngineColor
import monad_core.engine.model.EngineColor.{HSL, RGB}
import monad_core.simulator.application.engine.{DrawCommand, ShapeArchitect}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

object ShapePainter:

  extension (engineColor: EngineColor)

    private def toColor: Color =
      engineColor match
        case RGB(r, g, b) => Color.rgb(r.value, g.value, b.value)
        case HSL(h, s, l) => Color.hsb(h.value, s.value / 100, l.value / 100)

  def paint(gc: GraphicsContext)(using drawer: ShapeArchitect): Unit =
    val commands = drawer.drainBuffer()

    gc.clearRect(0, 0, gc.canvas.getWidth, gc.canvas.getHeight)

    commands.foreach:
      case DrawCommand.Circle(x, y, r, c) =>
        gc.fill = c.toColor
        gc.fillOval(x - r, y - r, r * 2, r * 2)
      case DrawCommand.Rectangle(x, y, w, h, c) =>
        gc.fill = c.toColor
        gc.fillRect(x - w / 2, y - h / 2, w, h)
