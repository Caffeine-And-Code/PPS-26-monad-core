package monad_core.engine.model

/**
 * Geometric shape of a [[Locatable]].
 *
 * Instances are created through [[Shape2D.circle]] and
 * [[Shape2D.rectangle]], which enforce positive dimensions.
 */
enum Shape2D:
  /**
   * Circle described by its radius.
   *
   * @param radius strictly positive radius
   */
  case Circle private[model] (radius: Double)

  /**
   * Rectangle described by its height and length
   *
   * @param height strictly positive height
   * @param length strictly positive length
   */
  case Rectangle private[model] (height: Double, length: Double)

/** Validated constructors and derived measurements for [[Shape2D]]. */
object Shape2D:

  /**
   * Creates a circle with a strictly positive radius.
   *
   * @param radius circle radius
   * @return the circle, or [[RadiusMustBeGreaterThanZero]] for a non-positive radius
   */
  def circle(radius: Double): Either[EngineError, Circle] =
    if radius <= 0 then Left(RadiusMustBeGreaterThanZero())
    else Right(Shape2D.Circle(radius))

  /**
   * Creates a rectangle with strictly positive dimensions.
   *
   * @param height rectangle height
   * @param length rectangle length
   * @return the rectangle, [[HeightMustBeGreaterThanZero]] for a non-positive height, or
   *   [[LengthMustBeGreaterThanZero]] for a non-positive length
   */
  def rectangle(height: Double, length: Double): Either[EngineError, Rectangle] =
    if height <= 0 then Left(HeightMustBeGreaterThanZero())
    else if length <= 0 then Left(LengthMustBeGreaterThanZero())
    else Right(Shape2D.Rectangle(height, length))

  extension (rectangle: Rectangle)
    /** @return half of the rectangle length */
    def halfLength: Double = rectangle.length / 2

    /** @return half of the rectangle height */
    def halfHeight: Double = rectangle.height / 2
