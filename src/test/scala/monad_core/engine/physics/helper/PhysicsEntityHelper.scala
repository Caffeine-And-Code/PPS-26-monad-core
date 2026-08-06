package monad_core.engine.physics.helper

import monad_core.engine.model.{Entity, Surface, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable

private[physics] object PhysicsEntityHelper:

  def makeFixedEntityCircle(
              id: String = "entity",
              position: Vector2D = Vector2D(0, 0),
              radius: Double = PhysicsConstantHelper.DefaultRadius
            ): Entity =
    Entity.circle(id, position, radius).value

  def makeMovingEntityCircle(
                    id: String = "entity",
                    position: Vector2D = Vector2D(0, 0),
                    radius: Double = PhysicsConstantHelper.DefaultRadius,
                    speed: Vector2D = Vector2D(0, 0)
                  ): Entity =
    makeFixedEntityCircle(id, position, radius).withSpeed(speed).value

  def makeMovingEntityRectangle(
                              id: String = "entity",
                              position: Vector2D = Vector2D(0, 0),
                              width: Double = PhysicsConstantHelper.DefaultDimension,
                              height: Double = PhysicsConstantHelper.DefaultDimension,
                              speed: Vector2D = Vector2D(0, 0)
                            ): Entity =
    Entity.rectangle(id, position, width, height).value.withSpeed(speed).value