package monad_core.simulator.presentation.components.forms.base

import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.FormDialog.matchToResult
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{
  CircleLabel,
  RectangleLabel
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class FormDialogTest extends AnyFunSuite with Matchers with MockFactory:

  test("if matchToResult is applied to an EngineError, the onError function is called"):
    val expectedError: BaseError       = mock[BaseError]
    val either: Either[BaseError, Int] = Left(expectedError)
    val onError                        = mockFunction[BaseError, Unit]
    val onSuccess                      = mockFunction[Int, Unit]

    onError.expects(expectedError).once()
    onSuccess.expects(*).never()

    either.matchToResult(onError)(onSuccess)

  test("if matchToResult is applied to the desired result, the onRightResult function is called"):
    val expectedResult: Int            = 10
    val either: Either[BaseError, Int] = Right(expectedResult)
    val onError                        = mockFunction[BaseError, Unit]
    val onSuccess                      = mockFunction[Int, Unit]

    onError.expects(*).never()
    onSuccess.expects(expectedResult).once()

    either.matchToResult(onError)(onSuccess)

  // TODO: this will probably make a good usage of Prolog
  test("buildShapeFields should have the correct architecture foreach possible value"):
    val expectedTotalSize            = 2
    val expectedCircleFieldNumber    = 1
    val expectedRectangleFieldNumber = 2
    val radiusFieldIndex             = 0
    val widthFieldIndex              = 0
    val heightFieldIndex             = 1
    val cases = Table(
      ("radius", "width", "height"),
      (None, None, None),
      (Some("4"), None, None),
      (Some("4"), Some("5"), None),
      (Some("4"), Some("5"), Some("6")),
      (None, Some("5"), Some("6")),
      (None, None, Some("6")),
      (None, Some("5"), None)
    )

    forAll(cases): (radius, width, height) =>
      val expectedRadiusField = TextFieldSpec(
        id = BaseFormParser.RadiusKey,
        label = "Radius",
        defaultValue = radius
      )
      val expectedWidthField = TextFieldSpec(
        id = BaseFormParser.LengthKey,
        label = "Width",
        defaultValue = width
      )
      val expectedHeightField = TextFieldSpec(
        id = BaseFormParser.HeightKey,
        label = "Height",
        defaultValue = height
      )

      val fields = FormDialog.buildShapeFields(radius, width, height)

      val circleFields    = fields.getOrElse(CircleLabel, Seq.empty).toList
      val rectangleFields = fields.getOrElse(RectangleLabel, Seq.empty).toList

      fields.size should be(expectedTotalSize)
      circleFields.length should be(expectedCircleFieldNumber)
      rectangleFields.length should be(expectedRectangleFieldNumber)
      circleFields(radiusFieldIndex) should be(expectedRadiusField)
      rectangleFields(widthFieldIndex) should be(expectedWidthField)
      rectangleFields(heightFieldIndex) should be(expectedHeightField)
