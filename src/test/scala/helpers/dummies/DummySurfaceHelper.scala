package helpers.dummies

import monad_core.engine.helper.PhysicsConstantHelper
import monad_core.engine.model.{Surface, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable

/** A helper object for creating dummy surfaces for testing purposes. */
object DummySurfaceHelper:

  private val DefaultDimension = 1.0

  /**
   * Creates a validated circular surface fixture.
   *
   * @param id
   *   surface identifier
   * @param position
   *   world position
   * @param radius
   *   circle radius
   * @return
   *   circular surface fixture
   */
  def makeSurfaceCircle(
      id: String = "surface",
      position: Vector2D = Vector2D(0.0, 0.0),
      radius: Double = DefaultDimension
  ): Surface =
    Surface.circle(id, position, radius).value
