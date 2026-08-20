package monad_core.engine.helper

import monad_core.engine.model.{Surface, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable

private[engine] object DummySurfaceHelper:

  def makeSurfaceCircle(
      id: String = "surface",
      position: Vector2D = Vector2D(0.0, 0.0),
      radius: Double = PhysicsConstantHelper.DefaultRadius
  ): Surface =
    Surface.circle(id, position, radius).value
