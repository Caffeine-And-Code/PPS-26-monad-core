package monad_core.engine.helper

import monad_core.engine.model.{Entity, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable

private[engine] object DummyEntityHelper:

  def makeFixedEntityCircle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      radius: Double = PhysicsConstantHelper.DefaultRadius,
      rotation: Double = 0.0
  ): Entity =
    Entity
      .circle(id = id, position = Vector2D(0, 0), radius = radius, rotation = rotation)
      .value
      .moveTo(position)

  def makeMovingEntityCircle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      radius: Double = PhysicsConstantHelper.DefaultRadius,
      speed: Vector2D = Vector2D(0, 0),
      rotation: Double = 0.0
  ): Entity =
    makeFixedEntityCircle(id = id, position = position, radius = radius, rotation = rotation)
      .withSpeed(Some(speed))

  def makeFixedEntityRectangle(
      id: String = "entity",
      position: Vector2D = Vector2D(0, 0),
      width: Double = PhysicsConstantHelper.DefaultDimension,
      height: Double = PhysicsConstantHelper.DefaultDimension,
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
      width: Double = PhysicsConstantHelper.DefaultDimension,
      height: Double = PhysicsConstantHelper.DefaultDimension,
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
      .withSpeed(Some(speed))
