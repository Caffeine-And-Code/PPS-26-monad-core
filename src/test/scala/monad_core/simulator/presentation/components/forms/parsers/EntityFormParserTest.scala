package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Shape2D, Vector2D}
import monad_core.simulator.MissingKeyInFormError
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.{convertEitherToValuable, convertLeftProjectionToValuable}
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class EntityFormParserTest extends AnyFunSuite with Inside with Matchers with MockFactory:
  val EntityRadius: Double = 5
  val EntityLength: Double = 6
  val EntityHeight: Double = 7
  val EntityPosition: Vector2D = Vector2D(10, 11)
  val EntityFormValues: Map[String, String] = Map(
    "x" -> EntityPosition.x.toString,
    "y" -> EntityPosition.y.toString
  )

  def buildShapeFormValues(shapeLabel: String): Map[String, String] =
    EntityFormValues + ("shape" -> shapeLabel)

  def circleFormValues: Map[String, String] =
    buildShapeFormValues(EntityShapes.CircleLabel)
      + ("radius" -> EntityRadius.toString)

  def rectangleFormValues: Map[String, String] =
    buildShapeFormValues(EntityShapes.RectangleLabel)
      + ("length" -> EntityLength.toString)
      + ("height" -> EntityHeight.toString)

  test("A circle entity can be converted from form values by utilizing the default id generator"):
    val expectedCircle = Shape2D.circle(EntityRadius).value
    val formValues = circleFormValues

    val parseResult = EntityFormParser.buildEntity(formValues)

    inside(parseResult):
      case Right(entity) =>
        entity.position should be(EntityPosition)
        entity.health should be(None)
        entity.speed should be(None)
        entity.teamId should be(None)
        entity.shape should be(expectedCircle)

  test("A rectangle entity can be converted from form values by utilizing the default id generator"):
    val expectedRectangle = Shape2D.rectangle(height = EntityHeight, length = EntityLength).value
    val formValues = rectangleFormValues

    val parseResult = EntityFormParser.buildEntity(formValues)

    inside(parseResult):
      case Right(entity) =>
        entity.position should be(EntityPosition)
        entity.health should be(None)
        entity.speed should be(None)
        entity.teamId should be(None)
        entity.shape should be(expectedRectangle)

  test("If form values doesn't have the 'shape' value the entity cannot be parsed"):
    val expectedError = MissingKeyInFormError("shape")
    val formValuesWithMissingShape = EntityFormValues

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingShape)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'x' value the entity cannot be parsed"):
    val expectedError = MissingKeyInFormError("x")
    val formValuesWithMissingX = Map.empty[String, String]

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingX)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'y' value the entity cannot be parsed"):
    val expectedError = MissingKeyInFormError("y")
    val formValuesWithMissingY = Map("x" -> EntityPosition.x.toString)

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingY)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'radius' value the circle entity cannot be parsed"):
    val expectedError = MissingKeyInFormError("radius")
    val formValuesWithMissingRadius = circleFormValues - "radius"

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingRadius)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'height' value the rectangle entity cannot be parsed"):
    val expectedError = MissingKeyInFormError("height")
    val formValuesWithMissingHeight = rectangleFormValues - "height"

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingHeight)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'length' value the rectangle entity cannot be parsed"):
    val expectedError = MissingKeyInFormError("length")
    val formValuesWithMissingLength = rectangleFormValues - "length"

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingLength)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("A function that generates an id can be passed to the buildEntity function"):
    val formValuesTable = Table(
      "formValues",
      circleFormValues,
      rectangleFormValues
    )

    forAll(formValuesTable): formValues =>
      val expectedId = "id"
      val generateId = mockFunction[String]

      generateId.expects().returns(expectedId).once()

      val parseResult = EntityFormParser.buildEntity(formValues, generateId = generateId)

      inside(parseResult):
        case Right(entity) =>
          entity.id.value should be(expectedId)

  test("buildByShape should correctly construct entities based on shape type"):
    val testCases = Table(
      ("shape", "values", "expectedShape"),
      (
        EntityShapes.Circle,
        Map("radius" -> EntityRadius.toString),
        Shape2D.circle(EntityRadius).value
      ),
      (
        EntityShapes.Rectangle,
        Map("height" -> EntityHeight.toString, "length" -> EntityLength.toString),
        Shape2D.rectangle(height = EntityHeight, length = EntityLength).value
      )
    )

    forAll(testCases): (shape, values, expectedShape) =>
      val dummyId = "test-id"
      val result = EntityFormParser.buildByShape(shape, dummyId, EntityPosition, values)

      val entity = result.value
      entity.id.value should be(dummyId)
      entity.position should be(EntityPosition)
      entity.shape should be(expectedShape)

  test("buildByShape should return an error when shape-specific fields are missing or invalid"):
    val invalidCases = Table(
      ("shape", "values", "expectedKey"),
      (EntityShapes.Circle, Map.empty[String, String], "radius"),
      (EntityShapes.Rectangle, Map("length" -> "5"), "height"),
      (EntityShapes.Rectangle, Map("height" -> "5"), "length")
    )

    forAll(invalidCases): (shape, values, expectedKey) =>
      val result = EntityFormParser.buildByShape(shape, "id", EntityPosition, values)

      result.left.value should be(MissingKeyInFormError(expectedKey))