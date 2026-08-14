package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Shape2D, Vector2D}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{
  CircleLabel,
  RectangleLabel
}
import monad_core.simulator.{InvalidNumericFormFieldError, MissingKeyInFormError}

object BaseFormParser:
  val RadiusKey = "radius"
  val HeightKey = "height"
  val LengthKey = "length"

  private[forms] def parseDouble(
      values: Map[String, String],
      key: String
  ): Either[BaseError, Double] =
    values.getValueSafe(key).flatMap { valueStr =>
      valueStr.toDoubleOption.toRight(InvalidNumericFormFieldError(key))
    }

  private[forms] def getSafeVector2D(
      values: Map[String, String],
      xKey: String,
      yKey: String
  ): Either[BaseError, Vector2D] =
    for
      x <- BaseFormParser.parseDouble(values, xKey)
      y <- BaseFormParser.parseDouble(values, yKey)
    yield Vector2D(x, y)

  private[forms] def getOptionalVector2D(
      values: Map[String, String],
      xKey: String,
      yKey: String
  ): Option[Vector2D] =
    getSafeVector2D(values, xKey, yKey) match
      case Right(vector) => Some(vector)
      case Left(_)       => None

  private[forms] def getShape(
      formShape: String,
      values: Map[String, String]
  ): Either[BaseError, Shape2D] =
    formShape match
      case CircleLabel =>
        for
          radius <- parseDouble(values, RadiusKey)
          circle <- Shape2D.circle(radius).adaptError()
        yield circle

      case RectangleLabel =>
        for
          height    <- parseDouble(values, HeightKey)
          length    <- parseDouble(values, LengthKey)
          rectangle <- Shape2D.rectangle(height, length).adaptError()
        yield rectangle

  extension (map: Map[String, String])

    private[forms] def getValueSafe(key: String): Either[BaseError, String] =
      map.get(key).toRight(MissingKeyInFormError(key))
