package monad_core.engine.model

/**
 * Rectangular extent of the simulated world.
 *
 * The upper-left corner is fixed at the origin, while the lower-right corner
 * stores the positive width and height.
 */
final class WorldBounds private (
    val upperLeft: Vector2D,
    val lowerRight: Vector2D
)

object WorldBounds:

  /** Creates bounds with strictly positive width and height. */
  def apply(width: Double, height: Double): Either[EngineError, WorldBounds] =
    if (width <= 0 || height <= 0) {
      Left(WorldBoundsCannotBeNegativeOrZero())
    } else {
      Right(
        new WorldBounds(
          upperLeft = Vector2D(0.0, 0.0),
          lowerRight = Vector2D(width, height)
        )
      )
    }

  /** Default `100 x 100` world bounds. */
  val default: WorldBounds =
    new WorldBounds(
      upperLeft = Vector2D(0.0, 0.0),
      lowerRight = Vector2D(100.0, 100.0)
    )
