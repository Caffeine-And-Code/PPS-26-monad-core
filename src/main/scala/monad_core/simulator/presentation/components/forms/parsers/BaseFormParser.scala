package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.Vector2D
import monad_core.simulator.domain.engine.MonadCoreShape
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{CircleLabel, RectangleLabel}
import monad_core.simulator.{InvalidNumericFormFieldError, MissingKeyInFormError}

object BaseFormParser:
  val RadiusKey = "radius"
  val HeightKey = "height"
  val LengthKey = "length"
  
  private[forms] def parseDouble(values: Map[String, String], key: String): Either[BaseError, Double] =
    values.getValueSafe(key).flatMap { valueStr =>
      valueStr.toDoubleOption.toRight(InvalidNumericFormFieldError(key))
    }

  private[forms] def getSafeVector(values: Map[String, String], xKey: String, yKey: String): Either[BaseError, (Double, Double)] =
    for
      x <- BaseFormParser.parseDouble(values, xKey)
      y <- BaseFormParser.parseDouble(values, yKey)
    yield (x, y)

  private[forms] def getOptionalVector2D(values: Map[String, String], xKey: String, yKey: String): Option[(Double, Double)] =
    getSafeVector(values, xKey, yKey) match
      case Right(vector) => Some(vector)
      case Left(_) => None

  private[forms] def getShape(
                               formShape: String,
                               values: Map[String, String]
                             ): Either[BaseError, MonadCoreShape] =
    formShape match
      case CircleLabel =>
        for
          radius <- parseDouble(values, RadiusKey)
        yield SimulationCircle(radius)

      case RectangleLabel =>
        for
          height <- parseDouble(values, HeightKey)
          length <- parseDouble(values, LengthKey)
        yield SimulationRectangle(length, height)
  
  extension (map: Map[String, String])
    private[forms] def getValueSafe(key: String): Either[BaseError, String] =
      map.get(key).toRight(MissingKeyInFormError(key))