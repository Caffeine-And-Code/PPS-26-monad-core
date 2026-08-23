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

object EntityFormParser:

  val PositionXKey    = "x"
  val PositionYKey    = "y"
  val ShapeKey        = "shape"
  val SpeedXKey       = "speedX"
  val SpeedYKey       = "speedY"
  val RotationKey     = "rotation"
  val AngularSpeedKey = "angularSpeed"
  val HealthKey       = "health"
  val WeightKey       = "weight"
  val TeamIdKey       = "teamId"

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

      entityWithSpeed = BaseFormParser.getOptionalVector2D(values, SpeedXKey, SpeedYKey) match
        case Some(vector) => entity.withSpeed(vector)
        case _            => entity

      angularSpeed <- BaseFormParser.parseOptionalDouble(values, AngularSpeedKey)
      entityWithAngularSpeed = angularSpeed.fold(entityWithSpeed)(entityWithSpeed.withAngularSpeed)

      health = values.get(HealthKey).flatMap(_.toDoubleOption).map(_.toInt)
      entityWithHealth <- health match
        case Some(h) => entityWithAngularSpeed.withHealth(h).adaptError()
        case None    => Right(entityWithAngularSpeed)

      weight = values.get(WeightKey).flatMap(_.toDoubleOption).map(_.toInt)
      entityWithWeight <- weight match
        case Some(w) => entityWithHealth.withWeight(w).adaptError()
        case None    => Right(entityWithHealth)

      teamId = values.get(TeamIdKey)
      finalEntity <- teamId match
        case Some(id) =>
          if id.isEmpty then Right(entityWithWeight)
          else entityWithWeight.withTeamId(id).adaptError()

        case None => Right(entityWithWeight)
    yield finalEntity

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
