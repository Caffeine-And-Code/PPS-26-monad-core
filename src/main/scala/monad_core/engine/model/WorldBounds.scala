package monad_core.engine.model

final class WorldBounds private (
                        val upperLeft: Vector2D,
                        val lowerRight: Vector2D
                      )

object WorldBounds:

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

  val default: WorldBounds =
    new WorldBounds(
      upperLeft = Vector2D(0.0, 0.0),
      lowerRight = Vector2D(100.0, 100.0)
    )