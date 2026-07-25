package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Vector2D}
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

  extension (value: EntityShapes)
    def getStringValue: String =
      value match
        case Circle => CircleLabel
        case Rectangle => RectangleLabel


object EntityFormParser:

  def buildEntity(
                   values: Map[String, String],
                   generateId: () => String = () => Random.alphanumeric.take(10).mkString
                 ): Either[EngineError, Entity] =
    for
      x <- BaseFormParser.parseDouble(values, "x")
      y <- BaseFormParser.parseDouble(values, "y")
      position = Vector2D(x, y)
      shapeValueEither <- values.getValueSafe("shape")
      shapeValue <- shapeValueEither.getEnumValue
      entity <- buildByShape(shapeValue, generateId(), position, values)
    yield entity

  private[forms] def buildByShape(
                                   shape: EntityShapes,
                                   id: String,
                                   position: Vector2D,
                                   values: Map[String, String]
                                 ): Either[EngineError, Entity] =
    shape match
      case Circle =>
        for
          radius <- BaseFormParser.parseDouble(values, "radius")
          entity <- Entity.circle(id, position, radius)
        yield entity

      case Rectangle =>
        for
          height <- BaseFormParser.parseDouble(values, "height")
          length <- BaseFormParser.parseDouble(values, "length")
          entity <- Entity.rectangle(id, position, height, length)
        yield entity