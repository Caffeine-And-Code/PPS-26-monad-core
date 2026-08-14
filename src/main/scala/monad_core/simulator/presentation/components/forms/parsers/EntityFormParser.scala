package monad_core.simulator.presentation.components.forms.parsers

import monad_core.simulator.domain.engine.MonadCoreEntity
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe

import scala.util.Random

object EntityFormParser:

  val PositionXKey = "x"
  val PositionYKey = "y"
  val ShapeKey     = "shape"
  val SpeedXKey    = "speedX"
  val SpeedYKey    = "speedY"
  val HealthKey    = "health"
  val WeightKey    = "weight"
  val TeamIdKey    = "teamId"

  def buildEntity(
      values: Map[String, String],
      generateId: () => String = () => Random.alphanumeric.take(10).mkString
  ): Either[BaseError, MonadCoreEntity] =
    for
      position        <- BaseFormParser.getSafeVector(values, PositionXKey, PositionYKey)
      shapeFormChoice <- values.getValueSafe(ShapeKey)
      shape           <- BaseFormParser.getShape(shapeFormChoice, values)

      speed  = BaseFormParser.getOptionalVector2D(values, SpeedXKey, SpeedYKey)
      health = values.get(HealthKey).flatMap(_.toDoubleOption).map(_.toInt)
      weight = values.get(WeightKey).flatMap(_.toDoubleOption).map(_.toInt)
      teamId = values.get(TeamIdKey).filterNot(id => id.isEmpty)

      entity = MonadCoreEntity(
        id = generateId(),
        position = position,
        shape = shape,
        speed = speed,
        weight = weight,
        health = health,
        teamId = teamId
      )
    yield entity
