package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.Shape2D
import monad_core.simulator.InvalidShapeFormFieldError

private[forms] enum LocatableFormShapes:
  private[forms] case Circle
  private[forms] case Rectangle

private[forms] object LocatableFormShapes:
  val CircleLabel    = "Circle"
  val RectangleLabel = "Rectangle"

  extension (value: String)

    def getEnumValue: Either[EngineError, LocatableFormShapes] =
      value match
        case CircleLabel    => Right(Circle)
        case RectangleLabel => Right(Rectangle)
        case _              => Left(InvalidShapeFormFieldError("shape"))

  extension (shape: Shape2D)

    def getEnumValue: LocatableFormShapes =
      shape match
        case Shape2D.Circle(_)       => LocatableFormShapes.Circle
        case Shape2D.Rectangle(_, _) => LocatableFormShapes.Rectangle

    def getDefaultValuesByShape: (Option[String], Option[String], Option[String]) =
      shape match
        case Shape2D.Circle(r)       => (Some(r.toString), None, None)
        case Shape2D.Rectangle(h, l) => (None, Some(h.toString), Some(l.toString))

  extension (value: LocatableFormShapes)

    def getStringValue: String =
      value match
        case Circle    => CircleLabel
        case Rectangle => RectangleLabel
