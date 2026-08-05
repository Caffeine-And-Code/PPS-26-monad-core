package monad_core.simulator.presentation.components.forms.parsers

import monad_core.simulator.MissingKeyInFormError
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.MonadCoreSurface
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class SurfaceFormParserTest extends AnyFunSuite with Inside with Matchers with MockFactory:
  val SurfaceRadius: Double = 5
  val SurfaceLength: Double = 6
  val SurfaceHeight: Double = 7
  val SurfacePosition: (Double, Double) = (10, 11)
  val SurfaceAppliedForce: (Double, Double) = (12, 13)
  val SurfaceFrictionIndex: Double = 0.8
  val SurfaceFormValues: Map[String, String] = Map(
    SurfaceFormParser.PositionXKey -> SurfacePosition._1.toString,
    SurfaceFormParser.PositionYKey -> SurfacePosition._2.toString
  )

  def buildShapeFormValues(shapeLabel: String): Map[String, String] =
    SurfaceFormValues + (SurfaceFormParser.ShapeKey -> shapeLabel)

  def circleFormValues: Map[String, String] =
    buildShapeFormValues(LocatableFormShapes.CircleLabel)
      + (BaseFormParser.RadiusKey -> SurfaceRadius.toString)

  def rectangleFormValues: Map[String, String] =
    buildShapeFormValues(LocatableFormShapes.RectangleLabel)
      + (BaseFormParser.LengthKey -> SurfaceLength.toString)
      + (BaseFormParser.HeightKey -> SurfaceHeight.toString)

  def buildFormValuesWithOptionalParams(formValues: Map[String, String]): Map[String, String] =
    formValues
      + (SurfaceFormParser.FrictionIndexKey -> SurfaceFrictionIndex.toString)
      + (SurfaceFormParser.AppliedForceXKey -> SurfaceAppliedForce._1.toString)
      + (SurfaceFormParser.AppliedForceYKey -> SurfaceAppliedForce._2.toString)

  test("A circle surface can be converted from form values by utilizing the default id generator"):
    val expectedCircle = MonadCoreSurface("id", SurfacePosition, SimulationCircle(SurfaceRadius))
    val formValues = circleFormValues

    val parseResult = SurfaceFormParser.buildSurface(formValues)

    inside(parseResult):
      case Right(surface) =>
        surface.position should be(SurfacePosition)
        surface.frictionIndex should be(expectedCircle.frictionIndex)
        surface.appliedForce should be(expectedCircle.appliedForce)
        surface.shape should be(expectedCircle.shape)

  test("A rectangle surface can be converted from form values by utilizing the default id generator"):
    val expectedRectangle = MonadCoreSurface("id", SurfacePosition, SimulationRectangle(SurfaceLength, SurfaceHeight))
    val formValues = rectangleFormValues

    val parseResult = SurfaceFormParser.buildSurface(formValues)

    inside(parseResult):
      case Right(surface) =>
        surface.position should be(SurfacePosition)
        surface.frictionIndex should be(expectedRectangle.frictionIndex)
        surface.appliedForce should be(expectedRectangle.appliedForce)
        surface.shape should be(expectedRectangle.shape)

  test("If form values doesn't have the 'shape' value the surface cannot be parsed"):
    val expectedError = MissingKeyInFormError("shape")
    val formValuesWithMissingShape = SurfaceFormValues

    val parseResult = SurfaceFormParser.buildSurface(formValuesWithMissingShape)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'x' value the surface cannot be parsed"):
    val expectedError = MissingKeyInFormError(SurfaceFormParser.PositionXKey)
    val formValuesWithMissingX = Map.empty[String, String]

    val parseResult = SurfaceFormParser.buildSurface(formValuesWithMissingX)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'y' value the surface cannot be parsed"):
    val expectedError = MissingKeyInFormError(SurfaceFormParser.PositionYKey)
    val formValuesWithMissingY = Map(SurfaceFormParser.PositionXKey -> SurfacePosition._1.toString)

    val parseResult = SurfaceFormParser.buildSurface(formValuesWithMissingY)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have a Shape specific key the surface cannot be parsed"):
    val cases = Table(
      ("key", "baseFormValue"),
      (BaseFormParser.RadiusKey, circleFormValues),
      (BaseFormParser.HeightKey, rectangleFormValues),
      (BaseFormParser.LengthKey, rectangleFormValues),
    )

    forAll(cases): (key, baseFormValue) =>
      val expectedError = MissingKeyInFormError(key)
      val formValues = baseFormValue - key

      val parseResult = SurfaceFormParser.buildSurface(formValues)

      inside(parseResult):
        case Left(error) =>
          error should be(expectedError)

  test("A function that generates an id can be passed to the buildSurface function"):
    val formValuesTable = Table(
      "formValues",
      circleFormValues,
      rectangleFormValues
    )

    forAll(formValuesTable): formValues =>
      val expectedId = "id"
      val generateId = mockFunction[String]

      generateId.expects().returns(expectedId).once()

      val parseResult = SurfaceFormParser.buildSurface(formValues, generateId = generateId)

      inside(parseResult):
        case Right(surface) =>
          surface.id should be(expectedId)

  test("A complete surface form can be parsed to surface"):
    val cases = Table(
      "baseFormValues",
      circleFormValues,
      rectangleFormValues
    )

    forAll(cases): baseFormValues =>
      val fullCompletedFormValues = buildFormValuesWithOptionalParams(baseFormValues)

      val parseResult = SurfaceFormParser.buildSurface(fullCompletedFormValues)

      inside(parseResult):
        case Right(surface) =>
          surface.frictionIndex should be(Some(SurfaceFrictionIndex))
          surface.appliedForce should be(Some(SurfaceAppliedForce))

  test("A surface without friction and applied force values has them empty"):
    val cases = Table(
      "baseFormValues",
      circleFormValues,
      rectangleFormValues
    )

    forAll(cases): baseFormValues =>
      val parseResult = SurfaceFormParser.buildSurface(baseFormValues)

      inside(parseResult):
        case Right(surface) =>
          surface.frictionIndex should be(None)
          surface.appliedForce should be(None)