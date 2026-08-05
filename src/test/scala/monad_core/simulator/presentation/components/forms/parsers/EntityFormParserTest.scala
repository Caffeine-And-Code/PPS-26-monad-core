package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Shape2D, TeamId, Vector2D}
import monad_core.simulator.MissingKeyInFormError
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.{convertEitherToValuable, convertLeftProjectionToValuable}
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class EntityFormParserTest extends AnyFunSuite with Inside with Matchers with MockFactory:
  val Radius: Double = 5
  val Length: Double = 6
  val Height: Double = 7
  val Position: (Double, Double) = (10, 11)
  val Speed : (Double, Double) = (12, 13)
  val Weight: Double = 14
  val Health: Int = 15
  val TeamId: String = "teamIdValue"
  val FormValues: Map[String, String] = Map(
    EntityFormParser.PositionXKey -> Position._1.toString,
    EntityFormParser.PositionYKey -> Position._2.toString
  )

  def buildShapeFormValues(shapeLabel: String): Map[String, String] =
    FormValues + (EntityFormParser.ShapeKey -> shapeLabel)

  def circleFormValues: Map[String, String] =
    buildShapeFormValues(LocatableFormShapes.CircleLabel)
      + (BaseFormParser.RadiusKey -> Radius.toString)

  def rectangleFormValues: Map[String, String] =
    buildShapeFormValues(LocatableFormShapes.RectangleLabel)
      + (BaseFormParser.LengthKey -> Length.toString)
      + (BaseFormParser.HeightKey -> Height.toString)

  def buildFormValuesWithOptionalParams(formValues: Map[String, String]) : Map[String, String] =
    formValues
      + (EntityFormParser.TeamIdKey -> TeamId)
      + (EntityFormParser.SpeedXKey -> Speed._1.toString)
      + (EntityFormParser.SpeedYKey -> Speed._2.toString)
      + (EntityFormParser.WeightKey -> Weight.toString)
      + (EntityFormParser.HealthKey -> Health.toString)

  test("A circle entity can be converted from form values by utilizing the default id generator"):
    val expectedCircle = SimulationCircle(Radius)
    val formValues = circleFormValues

    val parseResult = EntityFormParser.buildEntity(formValues)

    inside(parseResult):
      case Right(entity) =>
        entity.position should be(Position)
        entity.health should be(None)
        entity.speed should be(None)
        entity.teamId should be(None)
        entity.shape should be(expectedCircle)

  test("A rectangle entity can be converted from form values by utilizing the default id generator"):
    val expectedRectangle = SimulationRectangle(height = Height, width = Length)
    val formValues = rectangleFormValues

    val parseResult = EntityFormParser.buildEntity(formValues)

    inside(parseResult):
      case Right(entity) =>
        entity.position should be(Position)
        entity.health should be(None)
        entity.speed should be(None)
        entity.teamId should be(None)
        entity.shape should be(expectedRectangle)

  test("If form values doesn't have the 'shape' value the entity cannot be parsed"):
    val expectedError = MissingKeyInFormError("shape")
    val formValuesWithMissingShape = FormValues

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
    val formValuesWithMissingY = Map(EntityFormParser.PositionXKey -> Position._1.toString)

    val parseResult = EntityFormParser.buildEntity(formValuesWithMissingY)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have a Shape specific key the entity cannot be parsed"):
    val cases = Table(
      ("key", "baseFormValue"),
      (BaseFormParser.RadiusKey, circleFormValues),
      (BaseFormParser.HeightKey, rectangleFormValues),
      (BaseFormParser.LengthKey, rectangleFormValues),
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
          entity.id should be(expectedId)

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
          entity.teamId should be(Some(TeamId))
          entity.health should be(Some(Health))
          entity.weight should be(Some(Weight))
          entity.speed should be(Some(Speed))