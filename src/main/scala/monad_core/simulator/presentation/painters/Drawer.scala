package monad_core.simulator.presentation.painters

import monad_core.engine.model.*
import monad_core.engine.public_api.Painter
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

import scala.collection.mutable.ListBuffer
import scala.math.{max, min}
import scala.util.Random
import scala.util.hashing.MurmurHash3

enum DrawCommand:
  case Circle(x: Double, y: Double, radius: Double, color: Color)
  case Rectangle(x: Double, y: Double, width: Double, height: Double, color: Color)

object Drawer extends Painter:

  private[painters] val buffer = ListBuffer.empty[DrawCommand]

  def getBuffer: ListBuffer[DrawCommand] = buffer

  override def baseColor: Color = Color.rgb(255, 255, 255)

  def teamIdColorRelation(id: TeamId): Color =
    val hash = MurmurHash3.stringHash(id.value)
    val rng = Random(hash)

    val hue = rng.nextDouble() * 360.0
    val saturation = 0.5 + (rng.nextDouble() * 0.5)
    val brightness = 0.5 + (rng.nextDouble() * 0.5)

    Color.hsb(hue, saturation, brightness)

  def drawCircle(locatable: Locatable, color: Color): Unit =
    locatable.shape match
      case Shape2D.Circle(r) =>
        buffer += DrawCommand.Circle(locatable.position.x, locatable.position.y, r, color)
      case _ => ()

  def drawRectangle(locatable: Locatable, color: Color): Unit =
    locatable.shape match
      case Shape2D.Rectangle(w, h) =>
        buffer += DrawCommand.Rectangle(locatable.position.x, locatable.position.y, w, h, color)
      case _ => ()

  def flush(gc: GraphicsContext): Unit =
    val commands = buffer.toList
    buffer.clear()
    gc.clearRect(0, 0, gc.canvas.getWidth, gc.canvas.getHeight)

    commands.foreach {
      case DrawCommand.Circle(x, y, r, c) =>
        gc.fill = c
        gc.fillOval(x - r, y - r, r * 2, r * 2)
      case DrawCommand.Rectangle(x, y, w, h, c) =>
        gc.fill = c
        gc.fillRect(x - w / 2, y - h / 2, w, h)
    }