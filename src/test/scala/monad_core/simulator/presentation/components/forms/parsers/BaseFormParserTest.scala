package monad_core.simulator.presentation.components.forms.parsers

import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe
import monad_core.simulator.{InvalidNumericFormFieldError, MissingKeyInFormError}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BaseFormParserTest extends AnyFunSuite with Inside with Matchers with MockFactory:
  val ValidFormValueKey = "valid_key"
  val InvalidFormValueKey = "invalid_key"
  val FormValues: Map[String, String] = Map(ValidFormValueKey -> "10.0")

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