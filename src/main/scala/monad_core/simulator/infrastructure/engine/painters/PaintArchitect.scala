package monad_core.simulator.infrastructure.engine.painters

import monad_core.engine.model.*
import monad_core.engine.model.EngineColor.{HSL, RGB}
import monad_core.engine.simulator.{DrawCommand, Painter}

import scala.util.Random
import scala.util.hashing.MurmurHash3

/**
 * Default pure `Painter` implementation supplied by the simulator.
 *
 * It assigns stable colors and translates engine locatables into backend-independent drawing commands.
 */
object PaintArchitect extends Painter:

  /** @return the validated white color used by entities without a team */
  def baseEntityColor: Either[EngineError, EngineColor] =
    for uniqueValue <- RGBValue(255)
    yield RGB(uniqueValue, uniqueValue, uniqueValue)

  /** @return the validated gray color shared by surfaces */
  def baseSurfaceColor: Either[EngineError, EngineColor] =
    for
      red   <- RGBValue(117)
      green <- RGBValue(117)
      blue  <- RGBValue(117)
    yield RGB(red, green, blue)

  /**
   * Derives a deterministic HSL color from a team identifier.
   *
   * @param id
   *   team identifier used as the random seed
   * @return
   *   the validated color associated with the identifier
   */
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

  /**
   * Describes a circular locatable as a drawing command.
   *
   * @param locatable
   *   entity or surface to describe
   * @param color
   *   fill color of the command
   * @return
   *   a circle command when the shape is circular, otherwise `None`
   */
  def drawCircle(locatable: Locatable, color: EngineColor): Option[DrawCommand] =
    locatable.shape match
      case Shape2D.Circle(r) =>
        Some(DrawCommand.Circle(locatable.position.x, locatable.position.y, r, color))
      case _ => None

  /**
   * Describes a rectangular locatable as a centered drawing command.
   *
   * @param locatable
   *   entity or surface to describe
   * @param color
   *   fill color of the command
   * @return
   *   a rectangle command preserving position and rotation, or `None` for another shape
   */
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
