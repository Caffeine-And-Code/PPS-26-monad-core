package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.Shape2D
import monad_core.simulator.InvalidShapeFormFieldError
import monad_core.simulator.errors.BaseError

/** Shape choices supported by entity and surface forms. */
private[forms] enum LocatableFormShapes:

  /** Circular form shape. */
  private[forms] case Circle

  /** Rectangular form shape. */
  private[forms] case Rectangle

/** Conversions between form labels, form shape choices and engine shapes. */
private[forms] object LocatableFormShapes:

  /** Label submitted for a circle. */
  val CircleLabel = "Circle"

  /** Label submitted for a rectangle. */
  val RectangleLabel = "Rectangle"

  extension (value: String)

    /**
     * Parses a submitted shape label.
     *
     * @return
     *   the corresponding form shape, or `InvalidShapeFormFieldError` for an unsupported label
     */
    def getEnumValue: Either[BaseError, LocatableFormShapes] =
      value match
        case CircleLabel    => Right(Circle)
        case RectangleLabel => Right(Rectangle)
        case _              => Left(InvalidShapeFormFieldError("shape"))

  extension (shape: Shape2D)

    /** Converts an engine shape to its form representation. */
    def getEnumValue: LocatableFormShapes =
      shape match
        case Shape2D.Circle(_)       => LocatableFormShapes.Circle
        case Shape2D.Rectangle(_, _) => LocatableFormShapes.Rectangle

    /**
     * Extracts textual dimension defaults from an engine shape.
     *
     * @return
     *   `(radius, height, length)`, with only dimensions relevant to the shape populated
     */
    def getDefaultValuesByShape: (Option[String], Option[String], Option[String]) =
      shape match
        case Shape2D.Circle(r)       => (Some(r.toString), None, None)
        case Shape2D.Rectangle(h, l) => (None, Some(h.toString), Some(l.toString))

  extension (value: LocatableFormShapes)

    /** Returns the label submitted for this form shape. */
    def getStringValue: String =
      value match
        case Circle    => CircleLabel
        case Rectangle => RectangleLabel
