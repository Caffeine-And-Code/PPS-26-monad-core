package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Surface, Vector2D}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{
  Circle,
  Rectangle,
  getEnumValue
}

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
  ): Either[BaseError, Surface] =
    for
      position         <- BaseFormParser.getSafeVector2D(values, PositionXKey, PositionYKey)
      shapeValueEither <- values.getValueSafe(ShapeKey)
      shapeValue       <- shapeValueEither.getEnumValue
      surface          <- buildByShape(shapeValue, generateId(), position, values)

      frictionIndex = values.get(FrictionIndexKey).flatMap(_.toDoubleOption)
      surfaceWithFriction <- frictionIndex match
        case Some(friction) => surface.withFrictionIndex(friction).adaptError()
        case None           => Right(surface)

      appliedForce = BaseFormParser.getOptionalVector2D(values, AppliedForceXKey, AppliedForceYKey)
      completeSurface <- appliedForce match
        case Some(force) => surfaceWithFriction.withAppliedForce(force).adaptError()
        case None        => Right(surfaceWithFriction)
    yield completeSurface

  private[forms] def buildByShape(
      shape: LocatableFormShapes,
      id: String,
      position: Vector2D,
      values: Map[String, String]
  ): Either[BaseError, Surface] =
    shape match
      case Circle =>
        for
          radius  <- BaseFormParser.parseDouble(values, BaseFormParser.RadiusKey)
          surface <- Surface.circle(id, position, radius).adaptError()
        yield surface

      case Rectangle =>
        for
          height  <- BaseFormParser.parseDouble(values, BaseFormParser.HeightKey)
          length  <- BaseFormParser.parseDouble(values, BaseFormParser.LengthKey)
          surface <- Surface.rectangle(id, position, height, length).adaptError()
        yield surface

}
