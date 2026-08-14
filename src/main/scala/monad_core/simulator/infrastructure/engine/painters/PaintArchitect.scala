package monad_core.simulator.infrastructure.engine.painters

import monad_core.engine.model.*
import monad_core.engine.model.EngineColor.{HSL, RGB}
import monad_core.engine.simulator.Painter
import monad_core.simulator.application.engine.{DrawCommand, ShapeArchitect}

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.hashing.MurmurHash3

object PaintArchitect extends Painter with ShapeArchitect:

  private val buffer = ListBuffer.empty[DrawCommand]

  def drainBuffer(): List[DrawCommand] =
    val commands = buffer.toList
    buffer.clear()
    commands

  def baseColor: Either[EngineError, EngineColor] =
    for uniqueValue <- RGBValue(255)
    yield RGB(uniqueValue, uniqueValue, uniqueValue)

  def teamIdColorRelation(id: TeamId): Either[EngineError, EngineColor] =
    val hash = MurmurHash3.stringHash(id.value)
    val rng  = Random(hash)
    val extractRandomValue: () => Int = () =>
      val randLowLimit = 50.0

      (randLowLimit + (rng.nextDouble() * randLowLimit)).toInt

    for
      hue        <- HueValue((rng.nextDouble() * 360.0).toInt)
      saturation <- PercentValue(extractRandomValue())
      brightness <- PercentValue(extractRandomValue())
    yield HSL(hue, saturation, brightness)

  def drawCircle(locatable: Locatable, color: EngineColor): Unit =
    locatable.shape match
      case Shape2D.Circle(r) =>
        buffer += DrawCommand.Circle(locatable.position.x, locatable.position.y, r, color)
      case _ => ()

  def drawRectangle(locatable: Locatable, color: EngineColor): Unit =
    locatable.shape match
      case Shape2D.Rectangle(w, h) =>
        buffer += DrawCommand.Rectangle(locatable.position.x, locatable.position.y, w, h, color)
      case _ => ()
