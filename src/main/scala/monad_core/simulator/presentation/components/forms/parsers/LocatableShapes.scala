package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.Shape2D
import monad_core.simulator.InvalidShapeFormFieldError

private[forms] enum LocatableShapes:
  private[forms] case Circle
  private[forms] case Rectangle

private[forms] object LocatableShapes:
  val CircleLabel = "Circle"
  val RectangleLabel = "Rectangle"

  extension (value: String)
    def getEnumValue: Either[EngineError, LocatableShapes] =
      value match
        case CircleLabel => Right(Circle)
        case RectangleLabel => Right(Rectangle)
        case _ => Left(InvalidShapeFormFieldError("shape"))

  extension (shape: Shape2D)
    def getEnumValue: LocatableShapes =
      shape match
        case Shape2D.Circle(_) => LocatableShapes.Circle
        case Shape2D.Rectangle(_, _) => LocatableShapes.Rectangle

  extension (value: LocatableShapes)
    def getStringValue: String =
      value match
        case Circle => CircleLabel
        case Rectangle => RectangleLabel