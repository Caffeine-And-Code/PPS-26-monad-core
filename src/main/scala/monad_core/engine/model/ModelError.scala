package monad_core.engine.model

case class CannotApplyDamageToNoneHealthEntity()
    extends EngineError("Cannot apply damage to none health entity")

case class HealthCannotBeNegativeOrZero(health: Double)
    extends EngineError(s"health cannot be negative or zero, health = $health")

case class CannotApplyNegativeDamage(damage: Double)
    extends EngineError(s"Cannot apply negative damage, damage = $damage")

case class PositionIsValid(position: Vector2D)
    extends EngineError(
      s"Position is invalid, x and y should be greater then 0, x = ${position.x}, y = ${position.y}"
    )

case class RotationMustBeAValidDegreeValue(rotation: Double) extends EngineError(s"Rotation must be between 0 and 360 degrees, rotation = $rotation")

case class LocatableIdCannotBeEmpty() extends EngineError("LocatableId cannot be empty")

case class RGBValueCannotExceedRange()
    extends EngineError("An RGB value cannot exceed the range of [0,255]")

case class HueValueCannotExceedRange()
    extends EngineError("An Hue value cannot exceed it's range of [0,360]")

case class PercentValueCannotExceedRange()
    extends EngineError("A Percent value cannot exceed it's range of [0,100]")

case class RadiusMustBeGreaterThanZero() extends EngineError("Radius must be greater than 0")

case class HeightMustBeGreaterThanZero() extends EngineError("Height must be greater than 0")

case class LengthMustBeGreaterThanZero() extends EngineError("Length must be greater than 0")

case class TeamIdCannotBeEmpty() extends EngineError("TeamId cannot be empty")

case class WeightCannotBeNegativeOrZero() extends EngineError("Weight cannot be negative")

case class ATeamCannotBeItsOwnEnemy() extends EngineError("A team cannot be its own enemy")

case class WorldBoundsCannotBeNegativeOrZero()
    extends EngineError("World bounds cannot be negative or zero")
