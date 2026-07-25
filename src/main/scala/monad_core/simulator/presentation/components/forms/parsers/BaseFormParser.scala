package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.simulator.{InvalidNumericFormFieldError, MissingKeyInFormError}

object BaseFormParser:
  private[forms] def parseDouble(values: Map[String, String], key: String): Either[EngineError, Double] =
    values.getValueSafe(key).flatMap { valueStr =>
      valueStr.toDoubleOption.toRight(InvalidNumericFormFieldError(key))
    }

  extension (map: Map[String, String])
    private[forms] def getValueSafe(key: String): Either[EngineError, String] =
      map.get(key).toRight(MissingKeyInFormError(key))