package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.Vector2D
import monad_core.simulator.errors.BaseError
import monad_core.simulator.{InvalidNumericFormFieldError, MissingKeyInFormError}

/** Shared conversion and lookup operations for submitted form values. */
object BaseFormParser:

  /** Key used by circle radius fields. */
  val RadiusKey = "radius"

  /** Key used by rectangle height fields. */
  val HeightKey = "height"

  /** Key used by rectangle length fields. */
  val LengthKey = "length"

  /**
   * Parses a required decimal value.
   *
   * @param values
   *   submitted values indexed by field identifier
   * @param key
   *   identifier of the value to parse
   * @return
   *   the parsed value, or a missing-key or invalid-numeric error
   */
  private[forms] def parseDouble(
      values: Map[String, String],
      key: String
  ): Either[BaseError, Double] =
    values.getValueSafe(key).flatMap { valueStr =>
      valueStr.toDoubleOption.toRight(InvalidNumericFormFieldError(key))
    }

  /**
   * Parses an optional decimal value.
   *
   * A missing or empty field is interpreted as `None`.
   *
   * @param values
   *   submitted values indexed by field identifier
   * @param key
   *   identifier of the value to parse
   * @return
   *   an optional parsed value, or an invalid-numeric error for non-empty malformed input
   */
  private[forms] def parseOptionalDouble(
      values: Map[String, String],
      key: String
  ): Either[BaseError, Option[Double]] =
    values.get(key).filter(_.nonEmpty) match
      case Some(value) =>
        value.toDoubleOption.map(Some.apply).toRight(InvalidNumericFormFieldError(key))
      case None => Right(None)

  /**
   * Parses an optional whole number representable as an `Int`.
   *
   * @param values
   *   submitted values indexed by field identifier
   * @param key
   *   identifier of the value to parse
   * @return
   *   an optional integer, or an invalid-numeric error for fractional, out-of-range or malformed input
   */
  private[forms] def parseOptionalInt(
      values: Map[String, String],
      key: String
  ): Either[BaseError, Option[Int]] =
    parseOptionalDouble(values, key).flatMap:
      case Some(value) if value.isWhole && value.isValidInt => Right(Some(value.toInt))
      case Some(_) => Left(InvalidNumericFormFieldError(key))
      case None    => Right(None)

  /**
   * Parses two required coordinates as a vector.
   *
   * @param values
   *   submitted values indexed by field identifier
   * @param xKey
   *   identifier of the horizontal coordinate
   * @param yKey
   *   identifier of the vertical coordinate
   * @return
   *   the parsed vector, or the first coordinate parsing error
   */
  private[forms] def getSafeVector2D(
      values: Map[String, String],
      xKey: String,
      yKey: String
  ): Either[BaseError, Vector2D] =
    for
      x <- BaseFormParser.parseDouble(values, xKey)
      y <- BaseFormParser.parseDouble(values, yKey)
    yield Vector2D(x, y)

  /**
   * Reads two coordinates as an optional vector.
   *
   * Any missing or invalid coordinate makes the complete vector absent; parsing errors are intentionally discarded.
   *
   * @param values
   *   submitted values indexed by field identifier
   * @param xKey
   *   identifier of the horizontal coordinate
   * @param yKey
   *   identifier of the vertical coordinate
   * @return
   *   the vector when both coordinates are valid, otherwise `None`
   */
  private[forms] def getOptionalVector2D(
      values: Map[String, String],
      xKey: String,
      yKey: String
  ): Option[Vector2D] =
    getSafeVector2D(values, xKey, yKey) match
      case Right(vector) => Some(vector)
      case Left(_)       => None

  extension (map: Map[String, String])

    /**
     * Retrieves a required value without treating an empty string as missing.
     *
     * @param key
     *   identifier of the value to retrieve
     * @return
     *   the stored value, or `MissingKeyInFormError` when the key is absent
     */
    private[forms] def getValueSafe(key: String): Either[BaseError, String] =
      map.get(key).toRight(MissingKeyInFormError(key))
