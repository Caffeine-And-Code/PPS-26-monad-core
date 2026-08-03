package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.Vector2D
import monad_core.simulator.errors.BaseError
import monad_core.simulator.{InvalidNumericFormFieldError, MissingKeyInFormError}

object BaseFormParser:
  private[forms] def parseDouble(values: Map[String, String], key: String): Either[BaseError, Double] =
    values.getValueSafe(key).flatMap { valueStr =>
      valueStr.toDoubleOption.toRight(InvalidNumericFormFieldError(key))
    }

  private[forms] def getSafeVector2D(values: Map[String, String], xKey: String, yKey: String): Either[BaseError, Vector2D] =
    for
      x <- BaseFormParser.parseDouble(values, xKey)
      y <- BaseFormParser.parseDouble(values, yKey)
    yield Vector2D(x, y)

  private[forms] def getOptionalVector2D(values: Map[String, String], xKey: String, yKey: String): Option[Vector2D] =
    getSafeVector2D(values, xKey, yKey) match
      case Right(vector) => Some(vector)
      case Left(_) => None

  extension (map: Map[String, String])
    private[forms] def getValueSafe(key: String): Either[BaseError, String] =
      map.get(key).toRight(MissingKeyInFormError(key))