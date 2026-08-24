package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.Vector2D
import monad_core.simulator.errors.BaseError
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

  private[forms] def parseOptionalDouble(
      values: Map[String, String],
      key: String
  ): Either[BaseError, Option[Double]] =
    values.get(key).filter(_.nonEmpty) match
      case Some(value) =>
        value.toDoubleOption.map(Some.apply).toRight(InvalidNumericFormFieldError(key))
      case None => Right(None)

  private[forms] def parseOptionalInt(
      values: Map[String, String],
      key: String
  ): Either[BaseError, Option[Int]] =
    parseOptionalDouble(values, key).flatMap:
      case Some(value) if value.isWhole && value.isValidInt => Right(Some(value.toInt))
      case Some(_) => Left(InvalidNumericFormFieldError(key))
      case None    => Right(None)

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

  extension (map: Map[String, String])

    private[forms] def getValueSafe(key: String): Either[BaseError, String] =
      map.get(key).toRight(MissingKeyInFormError(key))
