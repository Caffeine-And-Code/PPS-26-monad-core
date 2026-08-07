package monad_core.simulator.presentation.painters

import monad_core.simulator.application.engine.{DrawCommand, ShapeArchitect}
import scalafx.scene.canvas.GraphicsContext

object ShapePainter:

  def paint(gc: GraphicsContext)(using drawer: ShapeArchitect): Unit =
    val commands = drawer.drainBuffer()

    gc.clearRect(0, 0, gc.canvas.getWidth, gc.canvas.getHeight)

    commands.foreach:
      case DrawCommand.Circle(x, y, r, c) =>
        gc.fill = c
        gc.fillOval(x - r, y - r, r * 2, r * 2)
      case DrawCommand.Rectangle(x, y, w, h, c) =>
        gc.fill = c
        gc.fillRect(x - w / 2, y - h / 2, w, h)
  