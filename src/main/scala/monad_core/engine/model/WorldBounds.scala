package monad_core.engine.model

/**
 * Rectangular limits of the simulated world.
 *
 * @param upperLeft
 *   upper left world coordinates
 * @param lowerRight
 *   lower right world coordinates
 */
final class WorldBounds private (
    val upperLeft: Vector2D,
    val lowerRight: Vector2D
)

object WorldBounds:

  /** The horizontal coordinate of the origin of the world. */
  private val OriginX = 0.0

  /** The vertical coordinate of the origin of the world. */
  private val OriginY = 0.0

  /** Default side dimension of the world. */
  private val DefaultWorldDimension = 100.0

  /**
   * Creates bounds starting at the origin.
   *
   * @param width
   *   horizontal extent
   * @param height
   *   vertical extent
   * @return
   *   validated world bounds, or an invalid-dimension error
   */
  def apply(width: Double, height: Double): Either[EngineError, WorldBounds] =
    if (width <= 0 || height <= 0) {
      Left(WorldBoundsCannotBeNegativeOrZero())
    } else {
      Right(
        new WorldBounds(
          upperLeft = Vector2D(OriginX, OriginY),
          lowerRight = Vector2D(width, height)
        )
      )
    }

  /** Default world bounds. */
  val default: WorldBounds =
    new WorldBounds(
      upperLeft = Vector2D(OriginX, OriginY),
      lowerRight = Vector2D(DefaultWorldDimension, DefaultWorldDimension)
    )
