package monad_core.engine.model

/** Indicates that damage was applied to an entity without health. */
case class CannotApplyDamageToNoneHealthEntity()
    extends EngineError("Cannot apply damage to none health entity")

/** Raised when health is initialized or reduced to a non-positive value. */
case class HealthCannotBeNegativeOrZero(health: Double)
    extends EngineError(s"health cannot be negative or zero, health = $health")

/**
 * Indicates that a negative amount was supplied to a damage application.
 *
 * @param damage
 *   invalid negative amount
 */
case class CannotApplyNegativeDamage(damage: Double)
    extends EngineError(s"Cannot apply negative damage, damage = $damage")

/** Raised when a position contains a negative coordinate. */
case class PositionIsValid(position: Vector2D)
    extends EngineError(
      s"Position is invalid, x and y should be greater then 0, x = ${position.x}, y = ${position.y}"
    )

/** Raised when a rotation falls outside the inclusive range `[0, 360]`. */
case class RotationMustBeAValidDegreeValue(rotation: Double)
    extends EngineError(s"Rotation must be between 0 and 360 degrees, rotation = $rotation")

/** Raised when a locatable identifier is empty. */
case class LocatableIdCannotBeEmpty() extends EngineError("LocatableId cannot be empty")

/** Raised when an RGB component falls outside `[0, 255]`. */
case class RGBValueCannotExceedRange()
    extends EngineError("An RGB value cannot exceed the range of [0,255]")

/** Raised when a hue falls outside `[0, 360]`. */
case class HueValueCannotExceedRange()
    extends EngineError("An Hue value cannot exceed it's range of [0,360]")

/** Raised when a percentage falls outside `[0, 100]`. */
case class PercentValueCannotExceedRange()
    extends EngineError("A Percent value cannot exceed it's range of [0,100]")

/** Raised when a circle radius is not positive. */
case class RadiusMustBeGreaterThanZero() extends EngineError("Radius must be greater than 0")

/** Raised when a rectangle height is not positive. */
case class HeightMustBeGreaterThanZero() extends EngineError("Height must be greater than 0")

/** Raised when a rectangle length is not positive. */
case class LengthMustBeGreaterThanZero() extends EngineError("Length must be greater than 0")

/** Raised when a team identifier is empty. */
case class TeamIdCannotBeEmpty() extends EngineError("TeamId cannot be empty")

/** Raised when a weight is not positive. */
case class WeightCannotBeNegativeOrZero() extends EngineError("Weight cannot be negative or zero")

/** Indicates that a `Damage` value was constructed from a negative amount. */
case class DamageCannotBeNegative() extends EngineError("Damage cannot be negative")

/** Raised when a team includes its own identifier among its enemies. */
case class ATeamCannotBeItsOwnEnemy() extends EngineError("A team cannot be its own enemy")

/** Raised when a world dimension is not positive. */
case class WorldBoundsCannotBeNegativeOrZero()
    extends EngineError("World bounds cannot be negative or zero")
