package monad_core.simulator.presentation.components.forms.parsers

import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe
import monad_core.simulator.{InvalidNumericFormFieldError, MissingKeyInFormError}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class BaseFormParserTest extends AnyFunSuite with Inside with Matchers:
  val ValidFormValueKey = "valid_key"
  val InvalidFormValueKey = "invalid_key"
  val FormValues: Map[String, String] = Map(ValidFormValueKey -> "10.0")
  val XKey = "x"
  val YKey = "y"
  val VectorFormValues: Map[String, String] = Map(XKey -> "10.0", YKey -> "11.0")
  val Position: (Double,Double) = (10, 11)
  val Radius: Double = 5
  val Length: Double = 6
  val Height: Double = 7

  test("A value can be get safely"):
    val expectedValue = "10.0"

    val result = FormValues.getValueSafe(ValidFormValueKey)

    inside(result):
      case Right(value) =>
        value should be(expectedValue)

  test("getValueSafe returns a proper EngineError when getting an invalid form key"):
    val expectedError = MissingKeyInFormError(InvalidFormValueKey)

    val result = FormValues.getValueSafe(InvalidFormValueKey)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("A double value in a form Map can be converted correctly"):
    val convertedValue = 10.0

    val result = BaseFormParser.parseDouble(FormValues, ValidFormValueKey)

    inside(result):
      case Right(conversionResult) =>
        conversionResult shouldBe convertedValue

  test("Trying to convert a non-present key in map returns a specific error"):
    val expectedError = MissingKeyInFormError(InvalidFormValueKey)

    val result = BaseFormParser.parseDouble(FormValues, InvalidFormValueKey)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("Trying to convert an invalid value in map returns a specific error"):
    val expectedError = InvalidNumericFormFieldError(ValidFormValueKey)
    val FormValuesWithInvalidValue = Map(ValidFormValueKey -> "NotADouble")

    val result = BaseFormParser.parseDouble(FormValuesWithInvalidValue, ValidFormValueKey)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("A Vector2D can be obtained safely from a form Map"):
    val expectedVector : (Double,Double) = (10.0, 11.0)

    val result = BaseFormParser.getSafeVector(VectorFormValues, XKey, YKey)

    inside(result):
      case Right(vector) =>
        vector should be(expectedVector)

  test("getSafeVector2D returns a proper EngineError when the x key is missing"):
    val expectedError = MissingKeyInFormError(XKey)
    val formValuesWithMissingX = Map(YKey -> "11.0")

    val result = BaseFormParser.getSafeVector(formValuesWithMissingX, XKey, YKey)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("getSafeVector2D returns a proper EngineError when the y key is missing"):
    val expectedError = MissingKeyInFormError(YKey)
    val formValuesWithMissingY = Map(XKey -> "10.0")

    val result = BaseFormParser.getSafeVector(formValuesWithMissingY, XKey, YKey)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("getSafeVector2D returns a proper EngineError when the x value is not a valid number"):
    val expectedError = InvalidNumericFormFieldError(XKey)
    val formValuesWithInvalidX = Map(XKey -> "NotADouble", YKey -> "11.0")

    val result = BaseFormParser.getSafeVector(formValuesWithInvalidX, XKey, YKey)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("getSafeVector2D returns a proper EngineError when the y value is not a valid number"):
    val expectedError = InvalidNumericFormFieldError(YKey)
    val formValuesWithInvalidY = Map(XKey -> "10.0", YKey -> "NotADouble")

    val result = BaseFormParser.getSafeVector(formValuesWithInvalidY, XKey, YKey)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("getOptionalVector2D returns a Some when both x and y are present and valid"):
    val expectedVector : (Double,Double) = (10.0, 11.0)

    val result = BaseFormParser.getOptionalVector2D(VectorFormValues, XKey, YKey)

    result should be(Some(expectedVector))

  test("getOptionalVector2D returns None when the x key is missing"):
    val formValuesWithMissingX = Map(YKey -> "11.0")

    val result = BaseFormParser.getOptionalVector2D(formValuesWithMissingX, XKey, YKey)

    result should be(None)

  test("getOptionalVector2D returns None when the y key is missing"):
    val formValuesWithMissingY = Map(XKey -> "10.0")

    val result = BaseFormParser.getOptionalVector2D(formValuesWithMissingY, XKey, YKey)

    result should be(None)

  test("getOptionalVector2D returns None when both keys are missing"):
    val result = BaseFormParser.getOptionalVector2D(Map.empty, XKey, YKey)

    result should be(None)

  test("getOptionalVector2D returns None when the x value is not a valid number"):
    val formValuesWithInvalidX = Map(XKey -> "NotADouble", YKey -> "11.0")

    val result = BaseFormParser.getOptionalVector2D(formValuesWithInvalidX, XKey, YKey)

    result should be(None)

  test("getOptionalVector2D returns None when the y value is not a valid number"):
    val formValuesWithInvalidY = Map(XKey -> "10.0", YKey -> "NotADouble")

    val result = BaseFormParser.getOptionalVector2D(formValuesWithInvalidY, XKey, YKey)

    result should be(None)

  test("buildByShape should correctly construct entities based on shape type"):
    val testCases = Table(
      ("shape", "values", "expectedShape"),
      (
        LocatableFormShapes.Circle.getStringValue,
        Map(BaseFormParser.RadiusKey -> Radius.toString),
        SimulationCircle(Radius)
      ),
      (
        LocatableFormShapes.Rectangle.getStringValue,
        Map(BaseFormParser.HeightKey -> Height.toString, BaseFormParser.LengthKey -> Length.toString),
        SimulationRectangle(height = Height, width = Length)
      )
    )

    forAll(testCases): (shape, values, expectedShape) =>
      val dummyId = "test-id"

      val result = BaseFormParser.getShape(shape, values)

      inside(result):
        case Right(shape) =>
          shape should be(expectedShape)

  test("buildByShape should return an error when shape-specific fields are missing or invalid"):
    val invalidCases = Table(
      ("shape", "values", "expectedKey"),
      (LocatableFormShapes.Circle.getStringValue, Map.empty[String, String], "radius"),
      (LocatableFormShapes.Rectangle.getStringValue, Map("length" -> "5"), "height"),
      (LocatableFormShapes.Rectangle.getStringValue, Map("height" -> "5"), "length")
    )

    forAll(invalidCases): (shape, values, expectedKey) =>
      val result = BaseFormParser.getShape(shape, values)

      inside(result):
        case Left(error) =>
          error should be(MissingKeyInFormError(expectedKey))