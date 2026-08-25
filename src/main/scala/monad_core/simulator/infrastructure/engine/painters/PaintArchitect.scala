package monad_core.simulator.infrastructure.engine.painters

import monad_core.engine.model.*
import monad_core.engine.model.EngineColor.{HSL, RGB}
import monad_core.engine.simulator.{DrawCommand, Painter}

import scala.util.Random
import scala.util.hashing.MurmurHash3

object PaintArchitect extends Painter:

  def baseEntityColor: Either[EngineError, EngineColor] =
    for uniqueValue <- RGBValue(255)
    yield RGB(uniqueValue, uniqueValue, uniqueValue)

  def baseSurfaceColor: Either[EngineError, EngineColor] =
    for
      red   <- RGBValue(117)
      green <- RGBValue(117)
      blue  <- RGBValue(117)
    yield RGB(red, green, blue)

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

  def drawCircle(locatable: Locatable, color: EngineColor): Option[DrawCommand] =
    locatable.shape match
      case Shape2D.Circle(r) =>
        Some(DrawCommand.Circle(locatable.position.x, locatable.position.y, r, color))
      case _ => None

  def drawRectangle(locatable: Locatable, color: EngineColor): Option[DrawCommand] =
    locatable.shape match
      case Shape2D.Rectangle(w, h) =>
        Some(
          DrawCommand.Rectangle(
            locatable.position.x,
            locatable.position.y,
            h,
            w,
            locatable.rotation,
            color
          )
        )
      case _ => None
