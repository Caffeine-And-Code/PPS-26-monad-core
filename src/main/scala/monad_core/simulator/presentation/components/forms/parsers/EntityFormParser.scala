package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Shape2D, Vector2D}
import monad_core.simulator.InvalidShapeFormFieldError
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.errors.ErrorsAdapter.adapt
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{Circle, Rectangle, getEnumValue}

import scala.util.Random

object EntityFormParser:

  val PositionXKey = "x"
  val PositionYKey = "y"
  val ShapeKey = "shape"
  val SpeedXKey = "speedX"
  val SpeedYKey = "speedY"
  val HealthKey = "health"
  val WeightKey = "weight"
  val TeamIdKey = "teamId"
  val RadiusKey = "radius"
  val HeightKey = "height"
  val LengthKey = "length"

  def buildEntity(
                   values: Map[String, String],
                   generateId: () => String = () => Random.alphanumeric.take(10).mkString
                 ): Either[BaseError, Entity] =
    for
      position <- BaseFormParser.getSafeVector2D(values, PositionXKey, PositionYKey)
      shapeValueEither <- values.getValueSafe(ShapeKey)
      shapeValue <- shapeValueEither.getEnumValue
      entity <- buildByShape(shapeValue, generateId(), position, values)

      entityWithSpeed <- BaseFormParser.getOptionalVector2D(values, SpeedXKey, SpeedYKey) match
        case Some(vector) => entity.withSpeed(vector).adapt()
        case _ => Right(entity)

      health = values.get(HealthKey).flatMap(_.toDoubleOption).map(_.toInt)
      entityWithHealth <- health match
        case Some(h) => entityWithSpeed.withHealth(h).adapt()
        case None => Right(entityWithSpeed)

      weight = values.get(WeightKey).flatMap(_.toDoubleOption).map(_.toInt)
      entityWithWeight <- weight match
        case Some(w) => entityWithHealth.withWeight(w).adapt()
        case None => Right(entityWithHealth)

      teamId = values.get(TeamIdKey)
      finalEntity <- teamId match
        case Some(id) =>
          if id.isEmpty then
            Right(entityWithWeight)
          else
            entityWithWeight.withTeamId(id).adapt()

        case None => Right(entityWithWeight)
    yield finalEntity

  private[forms] def buildByShape(
                                   shape: LocatableFormShapes,
                                   id: String,
                                   position: Vector2D,
                                   values: Map[String, String]
                                 ): Either[BaseError, Entity] =
    shape match
      case Circle =>
        for
          radius <- BaseFormParser.parseDouble(values, RadiusKey)
          entity <- Entity.circle(id, position, radius).adapt()
        yield entity

      case Rectangle =>
        for
          height <- BaseFormParser.parseDouble(values, HeightKey)
          length <- BaseFormParser.parseDouble(values, LengthKey)
          entity <- Entity.rectangle(id, position, height, length).adapt()
        yield entity