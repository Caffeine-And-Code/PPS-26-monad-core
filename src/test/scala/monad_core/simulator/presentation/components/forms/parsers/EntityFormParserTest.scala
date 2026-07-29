package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Shape2D, TeamId, Vector2D}
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
  val EntitySpeed : Vector2D = Vector2D(12, 13)
  val EntityWeight: Double = 14
  val EntityHealth: Int = 15
  val EntityTeamId: String = "teamIdValue"
  val EntityFormValues: Map[String, String] = Map(
    EntityFormParser.PositionXKey -> EntityPosition.x.toString,
    EntityFormParser.PositionYKey -> EntityPosition.y.toString
  )

  def buildShapeFormValues(shapeLabel: String): Map[String, String] =
    EntityFormValues + (EntityFormParser.ShapeKey -> shapeLabel)

  def circleFormValues: Map[String, String] =
    buildShapeFormValues(LocatableShapes.CircleLabel)
      + (EntityFormParser.RadiusKey -> EntityRadius.toString)

  def rectangleFormValues: Map[String, String] =
    buildShapeFormValues(LocatableShapes.RectangleLabel)
      + (EntityFormParser.LengthKey -> EntityLength.toString)
      + (EntityFormParser.HeightKey -> EntityHeight.toString)

  def buildFormValuesWithOptionalParams(formValues: Map[String, String]) : Map[String, String] =
    formValues
      + (EntityFormParser.TeamIdKey -> EntityTeamId)
      + (EntityFormParser.SpeedXKey -> EntitySpeed.x.toString)
      + (EntityFormParser.SpeedYKey -> EntitySpeed.y.toString)
      + (EntityFormParser.WeightKey -> EntityWeight.toString)
      + (EntityFormParser.HealthKey -> EntityHealth.toString)

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
    val expectedError = MissingKeyInFormError(EntityFormParser.PositionXKey)
    val formValuesWithMissingX = Map.empty[String, String]

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingX)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'y' value the entity cannot be parsed"):
    val expectedError = MissingKeyInFormError(EntityFormParser.PositionYKey)
    val formValuesWithMissingY = Map(EntityFormParser.PositionXKey -> EntityPosition.x.toString)

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingY)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have a Shape specific key the entity cannot be parsed"):
    val cases = Table(
      ("key", "baseFormValue"),
      (EntityFormParser.RadiusKey, circleFormValues),
      (EntityFormParser.HeightKey, rectangleFormValues),
      (EntityFormParser.LengthKey, rectangleFormValues),
    )

    forAll(cases): (key, baseFormValue) =>
      val expectedError = MissingKeyInFormError(key)
      val formValues = baseFormValue - key

      val parseResult = EntityFormParser.buildEntity(formValues)

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

  test("A circle complete form can be parsed to entity"):
    val cases = Table(
      "baseFormValues",
      circleFormValues,
      rectangleFormValues
    )

    forAll(cases) : baseFormValues =>
      val fullCompletedFormValues = buildFormValuesWithOptionalParams(baseFormValues)

      val parseResult = EntityFormParser.buildEntity(fullCompletedFormValues)

      inside(parseResult):
        case Right(entity) =>
          entity.teamId should be(Some(TeamId(EntityTeamId).value))
          entity.health should be(Some(EntityHealth))
          entity.weight should be(Some(EntityWeight))
          entity.speed should be(Some(EntitySpeed))

  test("buildByShape should correctly construct entities based on shape type"):
    val testCases = Table(
      ("shape", "values", "expectedShape"),
      (
        LocatableShapes.Circle,
        Map(EntityFormParser.RadiusKey -> EntityRadius.toString),
        Shape2D.circle(EntityRadius).value
      ),
      (
        LocatableShapes.Rectangle,
        Map(EntityFormParser.HeightKey -> EntityHeight.toString, EntityFormParser.LengthKey -> EntityLength.toString),
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
      (LocatableShapes.Circle, Map.empty[String, String], "radius"),
      (LocatableShapes.Rectangle, Map("length" -> "5"), "height"),
      (LocatableShapes.Rectangle, Map("height" -> "5"), "length")
    )

    forAll(invalidCases): (shape, values, expectedKey) =>
      val result = EntityFormParser.buildByShape(shape, "id", EntityPosition, values)

      result.left.value should be(MissingKeyInFormError(expectedKey))