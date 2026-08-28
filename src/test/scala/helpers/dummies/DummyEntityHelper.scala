package helpers.dummies

import monad_core.engine.helper.PhysicsConstantHelper
import monad_core.engine.model.{Entity, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable

/** A helper object for creating dummy entities for testing purposes. */
object DummyEntityHelper:

  private val DefaultDimension = 1.0
  
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

  def makeMovingEntityCircle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      radius: Double = DefaultDimension,
      speed: Vector2D = Vector2D(0, 0),
      rotation: Double = 0.0
  ): Entity =
    makeFixedEntityCircle(id = id, position = position, radius = radius, rotation = rotation)
      .withSpeed(speed)

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
