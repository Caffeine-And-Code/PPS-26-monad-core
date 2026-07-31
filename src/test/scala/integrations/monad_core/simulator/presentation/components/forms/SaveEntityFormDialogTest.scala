package integrations.monad_core.simulator.presentation.components.forms

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*
import monad_core.simulator.presentation.components.forms.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.*

class SaveEntityFormDialogTest extends AnyFunSuite with Inside with Matchers with MockFactory with DialogTesting with FormTesting:
  val RadiusFieldIndex : Int = 2
  val WeightFieldIndex : Int = 5
  val HealthFieldIndex : Int = 6
  val GenericEitherCircleEntity: Either[EngineError, Entity] = Entity.circle("id", Vector2D(0, 0), 6)

  private def selectShapeInComboBox(shapeIndex: Int): Unit =
    val ShapeComboBoxIndex = 0
    allFormComboBoxes(ShapeComboBoxIndex).getSelectionModel.select(shapeIndex)

  private def selectCircleInComboBox(): Unit =
    selectShapeInComboBox(0)

  private def selectRectangleInComboBox(): Unit =
    selectShapeInComboBox(1)

  private val testTeams: Seq[Team] = Seq(
    Team(TeamId("RedTeam").value, Set.empty).value,
    Team(TeamId("BlueTeam").value, Set.empty).value
  )

  private def buildCompleteEntity(eitherEntity: Either[EngineError, Entity]) : Entity =
    val either = for
      entity <- eitherEntity
      entityWithHealth <- entity.withHealth(10)
      entityWithWeight <- entityWithHealth.withWeight(11)
      entityWithTeam <- entityWithWeight.withTeamId(testTeams.head.id.value)
      finalEntity <- entityWithTeam.withSpeed(Vector2D(12, 13))
    yield finalEntity

    either.value

  test("SaveEntityFormDialog opens successfully"):
    val props = SaveEntityFormDialogProps(
      title = "Create Entity",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    onFxThread {
      val result = SaveEntityFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveEntityFormDialog opens successfully by providing an entity to edit"):
    val props = SaveEntityFormDialogProps(
      title = "Edit Entity",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams,
      entityToUpdate = Some(GenericEitherCircleEntity.value)
    )

    onFxThread {
      val result = SaveEntityFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveEntityFormDialog invokes onSubmit with constructed Entity on valid input"):
    var submittedEntity: Option[Entity] = None

    val props = SaveEntityFormDialogProps(
      title = "Add Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = testTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))


      allFormFields(RadiusFieldIndex).setText("10.0")
      allFormFields(WeightFieldIndex).setText("70.0")
      allFormFields(HealthFieldIndex).setText("100.0")

      selectCircleInComboBox()

      formSaveButton.fire()
    }

    submittedEntity shouldBe defined

  test("SaveEntityFormDialog invokes onSubmit with constructed Entity on valid input, with passed entityToUpdate"):
    val entityToUpdate = buildCompleteEntity(GenericEitherCircleEntity)
    var submittedEntity: Option[Entity] = None
    val expectedRadius = 10.0
    val expectedWeight = 70.0
    val expectedHealth = 100.0

    val props = SaveEntityFormDialogProps(
      title = "Add Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = testTeams,
      entityToUpdate = Some(entityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      allFormFields(RadiusFieldIndex).setText(expectedRadius.toString)
      allFormFields(WeightFieldIndex).setText(expectedWeight.toString)
      allFormFields(HealthFieldIndex).setText(expectedHealth.toString)

      formSaveButton.fire()
    }

    submittedEntity shouldBe defined
    val providedEntity: Entity = submittedEntity.get
    providedEntity.id should be(entityToUpdate.id)
    providedEntity.position should be(entityToUpdate.position)

    providedEntity.health.get should be(expectedHealth)
    providedEntity.weight.get should be(expectedWeight)
    inside(providedEntity.shape ):
      case Shape2D.Circle(radius) => radius should be(expectedRadius)

  test("SaveEntityFormDialog invokes onError when form values are invalid"):
    var capturedError: Option[EngineError] = None

    val props = SaveEntityFormDialogProps(
      title = "Invalid Entity Test",
      onSubmit = _ => fail("onSubmit should not be called with invalid inputs"),
      onError = err => capturedError = Some(err),
      teams = testTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      formSaveButton.fire()
    }

    capturedError shouldBe defined

  test("SaveEntityFormDialog displays visually the circle entity values passed"):
    val circleEntityToUpdate: Entity = buildCompleteEntity(GenericEitherCircleEntity)
    var submittedEntity: Option[Entity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = testTeams,
      entityToUpdate = Some(circleEntityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("edit_circle_entity_form_dialog", rootNode, maxDiffPercentage = 10.0)
    }

  test("SaveEntityFormDialog displays architecturally the circle entity values passed"):
    val circleEntityToUpdate: Entity = buildCompleteEntity(Entity.circle("id", Vector2D(0, 0), 6))
    var submittedEntity: Option[Entity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = testTeams,
      entityToUpdate = Some(circleEntityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("edit_circle_entity_form_dialog", activeStage)
    }

  test("SaveEntityFormDialog displays visually the rectangle entity values passed"):
    val rectangleEntityToUpdate: Entity = buildCompleteEntity(Entity.rectangle("id", Vector2D(0, 0), 6, 10))
    var submittedEntity: Option[Entity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = testTeams,
      entityToUpdate = Some(rectangleEntityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("edit_rectangle_entity_form_dialog", rootNode, maxDiffPercentage = 10.0)
    }

  test("SaveEntityFormDialog displays architecturally the rectangle entity values passed"):
    val rectangleEntityToUpdate: Entity = buildCompleteEntity(Entity.rectangle("id", Vector2D(0, 0), 6, 10))
    var submittedEntity: Option[Entity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = testTeams,
      entityToUpdate = Some(rectangleEntityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("edit_rectangle_entity_form_dialog", activeStage)
    }

  test("SaveEntityFormDialog Circle matches visual snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectCircleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("save_circle_entity_form_dialog_initial", rootNode, maxDiffPercentage = 9.2)
    }


  test("SaveEntityFormDialog Circle matches architectural snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectCircleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("save_circle_entity_form_dialog_initial", activeStage)
    }

  test("SaveEntityFormDialog Rectangle matches visual snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectRectangleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("save_rectangle_entity_form_dialog_initial", rootNode, maxDiffPercentage = 9.2)
    }

  test("SaveEntityFormDialog Rectangle matches architectural snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectRectangleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("save_rectangle_entity_form_dialog_initial", activeStage)
    }