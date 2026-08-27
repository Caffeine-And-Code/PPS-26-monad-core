package monad_core.engine.model

/**
 * Geometric shape of a [[Locatable]].
 *
 * Instances are created through [[Shape2D.circle]] and
 * [[Shape2D.rectangle]], which enforce positive dimensions.
 */
enum Shape2D:
  /** A circle described by its radius. */
  case Circle private[model] (radius: Double)

  /** An axis-aligned rectangle described by its height and length. */
  case Rectangle private[model] (height: Double, length: Double)

object Shape2D:

  /** Creates a circle with a strictly positive radius. */
  def circle(radius: Double): Either[EngineError, Circle] =
    if radius <= 0 then Left(RadiusMustBeGreaterThanZero())
    else Right(Shape2D.Circle(radius))

  /** Creates a rectangle with strictly positive dimensions. */
  def rectangle(height: Double, length: Double): Either[EngineError, Rectangle] =
    if height <= 0 then Left(HeightMustBeGreaterThanZero())
    else if length <= 0 then Left(LengthMustBeGreaterThanZero())
    else Right(Shape2D.Rectangle(height, length))

  extension (rectangle: Rectangle)
    /** Returns half of the rectangle length. */
    def halfLength: Double = rectangle.length / 2

    /** Returns half of the rectangle height. */
    def halfHeight: Double = rectangle.height / 2
