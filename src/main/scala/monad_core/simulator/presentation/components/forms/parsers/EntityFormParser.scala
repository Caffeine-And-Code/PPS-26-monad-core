package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Entity, Vector2D}
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{
  Circle,
  Rectangle,
  getEnumValue
}

import scala.util.Random

/** Converts submitted entity form values into validated engine entities. */
object EntityFormParser:

  /** Key of the horizontal position. */
  val PositionXKey = "x"

  /** Key of the vertical position. */
  val PositionYKey = "y"

  /** Key of the selected shape. */
  val ShapeKey = "shape"

  /** Key of the optional horizontal speed. */
  val SpeedXKey = "speedX"

  /** Key of the optional vertical speed. */
  val SpeedYKey = "speedY"

  /** Key of the optional initial rotation. */
  val RotationKey = "rotation"

  /** Key of the optional angular speed. */
  val AngularSpeedKey = "angularSpeed"

  /** Key of the optional health. */
  val HealthKey = "health"

  /** Key of the optional contact damage. */
  val DamageKey = "damage"

  /** Key of the optional weight. */
  val WeightKey = "weight"

  /** Key of the optional team identifier. */
  val TeamIdKey = "teamId"

  /**
   * Builds an entity from submitted form values.
   *
   * Position and shape dimensions are required. Speed, rotation, angular speed, health, damage, weight and team are
   * applied when valid values are supplied. Domain construction failures are adapted to presentation errors.
   *
   * @param values
   *   submitted values indexed by the keys exposed by this parser and `BaseFormParser`
   * @param generateId
   *   identifier generator; invoked once after position and shape have been parsed
   * @return
   *   the validated entity, or the first parsing or domain error
   */
  def buildEntity(
      values: Map[String, String],
      generateId: () => String = () => Random.alphanumeric.take(10).mkString
  ): Either[BaseError, Entity] =
    for
      position         <- BaseFormParser.getSafeVector2D(values, PositionXKey, PositionYKey)
      shapeValueEither <- values.getValueSafe(ShapeKey)
      shapeValue       <- shapeValueEither.getEnumValue
      rotation         <- BaseFormParser.parseOptionalDouble(values, RotationKey)
      entity <- buildByShape(shapeValue, generateId(), position, values, rotation.getOrElse(0.0))

      entityWithSpeed = entity.withSpeed(
        BaseFormParser.getOptionalVector2D(values, SpeedXKey, SpeedYKey)
      )

      angularSpeed <- BaseFormParser.parseOptionalDouble(values, AngularSpeedKey)
      entityWithAngularSpeed = entityWithSpeed.withAngularSpeed(angularSpeed)

      health           <- BaseFormParser.parseOptionalInt(values, HealthKey)
      entityWithHealth <- entityWithAngularSpeed.withHealth(health).adaptError()

      damage           <- BaseFormParser.parseOptionalInt(values, DamageKey)
      entityWithDamage <- entityWithHealth.withDamage(damage).adaptError()

      weight           <- BaseFormParser.parseOptionalInt(values, WeightKey)
      entityWithWeight <- entityWithDamage.withWeight(weight).adaptError()

      teamId = values.get(TeamIdKey)
      finalEntity <- entityWithWeight.withTeamId(teamId.filter(_.nonEmpty)).adaptError()
    yield finalEntity

  /**
   * Builds the base entity for a parsed shape.
   *
   * @param shape
   *   shape selected by the user
   * @param id
   *   identifier assigned to the entity
   * @param position
   *   parsed initial position
   * @param values
   *   values containing the required shape dimensions
   * @param rotation
   *   initial rotation in degrees
   * @return
   *   a circle or rectangle entity, or a parsing or domain error
   */
  private[forms] def buildByShape(
      shape: LocatableFormShapes,
      id: String,
      position: Vector2D,
      values: Map[String, String],
      rotation: Double = 0.0
  ): Either[BaseError, Entity] =
    shape match
      case Circle =>
        for
          radius <- BaseFormParser.parseDouble(values, BaseFormParser.RadiusKey)
          entity <- Entity.circle(id, position, radius, rotation).adaptError()
        yield entity

      case Rectangle =>
        for
          height <- BaseFormParser.parseDouble(values, BaseFormParser.HeightKey)
          length <- BaseFormParser.parseDouble(values, BaseFormParser.LengthKey)
          entity <- Entity.rectangle(id, position, height, length, rotation).adaptError()
        yield entity
