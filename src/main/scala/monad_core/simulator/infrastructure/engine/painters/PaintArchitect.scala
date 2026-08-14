package monad_core.simulator.infrastructure.engine.painters

import monad_core.engine.model.*
import monad_core.engine.public_api.Painter
import monad_core.simulator.application.engine.{DrawCommand, ShapeArchitect}
import scalafx.scene.paint.Color

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.hashing.MurmurHash3

object PaintArchitect extends Painter with ShapeArchitect:

  private val buffer = ListBuffer.empty[DrawCommand]

  def drainBuffer(): List[DrawCommand] =
    val commands = buffer.toList
    buffer.clear()
    commands

  override def baseColor: Color = Color.rgb(255, 255, 255)

  def teamIdColorRelation(id: TeamId): Color =
    val hash = MurmurHash3.stringHash(id.value)
    val rng  = Random(hash)

    val hue        = rng.nextDouble() * 360.0
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
