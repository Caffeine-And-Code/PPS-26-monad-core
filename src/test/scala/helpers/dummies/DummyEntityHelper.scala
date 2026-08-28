package helpers.dummies

import monad_core.engine.helper.PhysicsConstantHelper
import monad_core.engine.model.{Entity, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable

/** A helper object for creating dummy entities for testing purposes. */
object DummyEntityHelper:

  private val DefaultDimension = 1.0

  /**
   * Creates a validated fixed circular entity.
   *
   * @param id
   *   entity identifier
   * @param position
   *   world position
   * @param radius
   *   circle radius
   * @param rotation
   *   rotation in degrees
   * @return
   *   fixed circular fixture
   */
  def makeFixedEntityCircle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      radius: Double = DefaultDimension,
      rotation: Double = 0.0
  ): Entity =
    Entity
      .circle(id = id, position = Vector2D(0, 0), radius = radius, rotation = rotation)
      .value
      .moveTo(position)

  /**
   * Creates a validated moving circular entity.
   *
   * @param id
   *   entity identifier
   * @param position
   *   world position
   * @param radius
   *   circle radius
   * @param speed
   *   initial linear velocity
   * @param rotation
   *   rotation in degrees
   * @return
   *   moving circular fixture
   */
  def makeMovingEntityCircle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      radius: Double = DefaultDimension,
      speed: Vector2D = Vector2D(0, 0),
      rotation: Double = 0.0
  ): Entity =
    makeFixedEntityCircle(id = id, position = position, radius = radius, rotation = rotation)
      .withSpeed(speed)

  /**
   * Creates a validated fixed rectangular entity.
   *
   * @param id
   *   entity identifier
   * @param position
   *   world position
   * @param width
   *   rectangle length
   * @param height
   *   rectangle height
   * @param rotation
   *   rotation in degrees
   * @return
   *   fixed rectangular fixture
   */
  def makeFixedEntityRectangle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      width: Double = DefaultDimension,
      height: Double = DefaultDimension,
      rotation: Double = 0.0
  ): Entity =
    Entity
      .rectangle(
        id = id,
        position = Vector2D(0, 0),
        length = width,
        height = height,
        rotation = rotation
      )
      .value
      .moveTo(position)

  /**
   * Creates a validated moving rectangular entity.
   *
   * @param id
   *   entity identifier
   * @param position
   *   world position
   * @param width
   *   rectangle length
   * @param height
   *   rectangle height
   * @param speed
   *   initial linear velocity
   * @param rotation
   *   rotation in degrees
   * @return
   *   moving rectangular fixture
   */
  def makeMovingEntityRectangle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      width: Double = DefaultDimension,
      height: Double = DefaultDimension,
      speed: Vector2D = Vector2D(0, 0),
      rotation: Double = 0.0
  ): Entity =
    makeFixedEntityRectangle(
      id = id,
      position = position,
      width = width,
      height = height,
      rotation = rotation
    )
      .withSpeed(speed)
