package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Surface, Vector2D}
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{
  Circle,
  Rectangle,
  getEnumValue
}

import scala.util.Random

/** Converts submitted surface form values into validated engine surfaces. */
object SurfaceFormParser {

  /** Key of the horizontal position. */
  val PositionXKey = "x"

  /** Key of the vertical position. */
  val PositionYKey = "y"

  /** Key of the selected shape. */
  val ShapeKey = "shape"

  /** Key of the optional initial rotation. */
  val RotationKey = "rotation"

  /** Key of the optional friction index. */
  val FrictionIndexKey = "friction"

  /** Key of the optional horizontal applied force. */
  val AppliedForceXKey = "appliedForceX"

  /** Key of the optional vertical applied force. */
  val AppliedForceYKey = "appliedForceY"

  /** Key of the optional damage applied over time. */
  val DamageOverTimeKey = "damageOverTime"

  /**
   * Builds a surface from submitted form values.
   *
   * Position and shape dimensions are required. Rotation, friction, applied force and damage over time are applied
   * when valid values are supplied. Domain construction failures are adapted to presentation errors.
   *
   * @param values
   *   submitted values indexed by the keys exposed by this parser and `BaseFormParser`
   * @param generateId
   *   identifier generator; invoked once after position and shape have been parsed
   * @return
   *   the validated surface, or the first parsing or domain error
   */
  def buildSurface(
      values: Map[String, String],
      generateId: () => String = () => Random.alphanumeric.take(10).mkString
  ): Either[BaseError, Surface] =
    for
      position         <- BaseFormParser.getSafeVector2D(values, PositionXKey, PositionYKey)
      shapeValueEither <- values.getValueSafe(ShapeKey)
      shapeValue       <- shapeValueEither.getEnumValue
      rotation         <- BaseFormParser.parseOptionalDouble(values, RotationKey)
      surface <- buildByShape(shapeValue, generateId(), position, values, rotation.getOrElse(0.0))

      frictionIndex = values.get(FrictionIndexKey).flatMap(_.toDoubleOption)
      surfaceWithFriction <- frictionIndex match
        case Some(friction) => surface.withFrictionIndex(friction).adaptError()
        case None           => Right(surface)

      appliedForce = BaseFormParser.getOptionalVector2D(values, AppliedForceXKey, AppliedForceYKey)
      surfaceWithAppliedForce <- appliedForce match
        case Some(force) => surfaceWithFriction.withAppliedForce(force).adaptError()
        case None        => Right(surfaceWithFriction)

      damageOverTime <- BaseFormParser.parseOptionalInt(values, DamageOverTimeKey)
      completeSurface <- damageOverTime match
        case Some(damage) => surfaceWithAppliedForce.withDamageOverTime(damage).adaptError()
        case None         => Right(surfaceWithAppliedForce)
    yield completeSurface

  /**
   * Builds the base surface for a parsed shape.
   *
   * @param shape
   *   shape selected by the user
   * @param id
   *   identifier assigned to the surface
   * @param position
   *   parsed initial position
   * @param values
   *   values containing the required shape dimensions
   * @param rotation
   *   initial rotation in degrees
   * @return
   *   a circle or rectangle surface, or a parsing or domain error
   */
  private[forms] def buildByShape(
      shape: LocatableFormShapes,
      id: String,
      position: Vector2D,
      values: Map[String, String],
      rotation: Double = 0.0
  ): Either[BaseError, Surface] =
    shape match
      case Circle =>
        for
          radius  <- BaseFormParser.parseDouble(values, BaseFormParser.RadiusKey)
          surface <- Surface.circle(id, position, radius, rotation).adaptError()
        yield surface

      case Rectangle =>
        for
          height  <- BaseFormParser.parseDouble(values, BaseFormParser.HeightKey)
          length  <- BaseFormParser.parseDouble(values, BaseFormParser.LengthKey)
          surface <- Surface.rectangle(id, position, height, length, rotation).adaptError()
        yield surface

}
