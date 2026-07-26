package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Shape2D, Vector2D}
import monad_core.simulator.InvalidShapeFormFieldError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe
import monad_core.simulator.presentation.components.forms.parsers.EntityShapes.{Circle, Rectangle, getEnumValue}

import scala.util.Random

private[forms] enum EntityShapes:
  private[forms] case Circle
  private[forms] case Rectangle

private[forms] object EntityShapes:
  val CircleLabel = "Circle"
  val RectangleLabel = "Rectangle"

  extension (value: String)
    def getEnumValue: Either[EngineError, EntityShapes] =
      value match
        case CircleLabel => Right(Circle)
        case RectangleLabel => Right(Rectangle)
        case _ => Left(InvalidShapeFormFieldError("shape"))

  extension (shape: Shape2D)
    def getEnumValue: EntityShapes =
      shape match
        case Shape2D.Circle(_) => EntityShapes.Circle
        case Shape2D.Rectangle(_, _) => EntityShapes.Rectangle

  extension (value: EntityShapes)
    def getStringValue: String =
      value match
        case Circle => CircleLabel
        case Rectangle => RectangleLabel


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
                 ): Either[EngineError, Entity] =
    for
      x <- BaseFormParser.parseDouble(values, PositionXKey)
      y <- BaseFormParser.parseDouble(values, PositionYKey)
      position = Vector2D(x, y)
      shapeValueEither <- values.getValueSafe(ShapeKey)
      shapeValue <- shapeValueEither.getEnumValue
      entity <- buildByShape(shapeValue, generateId(), position, values)

      speedX = values.get(SpeedXKey).flatMap(_.toDoubleOption)
      speedY = values.get(SpeedYKey).flatMap(_.toDoubleOption)
      entityWithSpeed <- (speedX, speedY) match
        case (Some(x), Some(y)) => entity.withSpeed(Vector2D(x, y))
        case _ => Right(entity)

      health = values.get(HealthKey).flatMap(_.toDoubleOption).map(_.toInt)
      entityWithHealth <- health match
        case Some(h) => entityWithSpeed.withHealth(h)
        case None => Right(entityWithSpeed)

      weight = values.get(WeightKey).flatMap(_.toDoubleOption).map(_.toInt)
      entityWithWeight <- weight match
        case Some(w) => entityWithHealth.withWeight(w)
        case None => Right(entityWithHealth)

      teamId = values.get(TeamIdKey)
      finalEntity <- teamId match
        case Some(id) => entityWithWeight.withTeamId(id)
        case None => Right(entityWithWeight)
    yield finalEntity

  private[forms] def buildByShape(
                                   shape: EntityShapes,
                                   id: String,
                                   position: Vector2D,
                                   values: Map[String, String]
                                 ): Either[EngineError, Entity] =
    shape match
      case Circle =>
        for
          radius <- BaseFormParser.parseDouble(values, RadiusKey)
          entity <- Entity.circle(id, position, radius)
        yield entity

      case Rectangle =>
        for
          height <- BaseFormParser.parseDouble(values, HeightKey)
          length <- BaseFormParser.parseDouble(values, LengthKey)
          entity <- Entity.rectangle(id, position, height, length)
        yield entity