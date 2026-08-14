package monad_core.simulator.presentation.components.forms.parsers

import monad_core.simulator.domain.engine.MonadCoreSurface
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe

import scala.util.Random

object SurfaceFormParser {
  val PositionXKey     = "x"
  val PositionYKey     = "y"
  val ShapeKey         = "shape"
  val FrictionIndexKey = "friction"
  val AppliedForceXKey = "appliedForceX"
  val AppliedForceYKey = "appliedForceY"

  def buildSurface(
      values: Map[String, String],
      generateId: () => String = () => Random.alphanumeric.take(10).mkString
  ): Either[BaseError, MonadCoreSurface] =
    for
      position        <- BaseFormParser.getSafeVector(values, PositionXKey, PositionYKey)
      shapeFormChoice <- values.getValueSafe(ShapeKey)
      shape           <- BaseFormParser.getShape(shapeFormChoice, values)
      frictionIndex = values.get(FrictionIndexKey).flatMap(_.toDoubleOption)
      appliedForce  = BaseFormParser.getOptionalVector2D(values, AppliedForceXKey, AppliedForceYKey)

      surface = MonadCoreSurface(
        id = generateId(),
        position = position,
        shape = shape,
        frictionIndex = frictionIndex,
        appliedForce = appliedForce
      )
    yield surface

}
