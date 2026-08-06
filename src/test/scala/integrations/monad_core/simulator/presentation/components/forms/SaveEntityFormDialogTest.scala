package integrations.monad_core.simulator.presentation.components.forms

import helpers.arrangers.ShapeKind.{Circle, Rectangle}
import helpers.arrangers.{MonadCoreEntityArranger, MonadCoreTeamArranger}
import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import monad_core.simulator.domain.engine.MonadCoreShape.SimulationCircle
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreTeam}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.*

class SaveEntityFormDialogTest extends AnyFunSuite with Inside with Matchers with MockFactory with DialogTesting with FormTesting:
  val RadiusFieldIndex: Int = 2
  val WeightFieldIndex: Int = 5
  val HealthFieldIndex: Int = 6
  val GenericCircleEntity: MonadCoreEntity = MonadCoreEntityArranger.arrangeRedEntity(Circle)
  val TestTeams: Seq[MonadCoreTeam] = MonadCoreTeamArranger.arrangeTeams

  private def selectShapeInComboBox(shapeIndex: Int): Unit =
    val ShapeComboBoxIndex = 0
    allFormComboBoxes(ShapeComboBoxIndex).getSelectionModel.select(shapeIndex)

  private def selectCircleInComboBox(): Unit =
    selectShapeInComboBox(0)

  private def selectRectangleInComboBox(): Unit =
    selectShapeInComboBox(1)

  test("SaveEntityFormDialog opens successfully"):
    val props = SaveEntityFormDialogProps(
      title = "Create Entity",
      onSubmit = _ => (),
      onError = _ => (),
      teams = TestTeams
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
      teams = TestTeams,
      entityToUpdate = Some(GenericCircleEntity)
    )

    onFxThread {
      val result = SaveEntityFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveEntityFormDialog invokes onSubmit with constructed Entity on valid input"):
    var submittedEntity: Option[MonadCoreEntity] = None

    val props = SaveEntityFormDialogProps(
      title = "Add Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = TestTeams
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
    val entityToUpdate = MonadCoreEntityArranger.arrangeRedEntity(Circle, withOptionals = true)
    var submittedEntity: Option[MonadCoreEntity] = None
    val expectedRadius = 10.0
    val expectedWeight = 70.0
    val expectedHealth = 100.0

    val props = SaveEntityFormDialogProps(
      title = "Add Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = TestTeams,
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
    val providedEntity: MonadCoreEntity = submittedEntity.get
    providedEntity.id should be(entityToUpdate.id)
    providedEntity.position should be(entityToUpdate.position)

    providedEntity.health.get should be(expectedHealth)
    providedEntity.weight.get should be(expectedWeight)
    inside(providedEntity.shape):
      case SimulationCircle(radius) => radius should be(expectedRadius)

  test("SaveEntityFormDialog invokes onError when form values are invalid"):
    var capturedError: Option[BaseError] = None

    val props = SaveEntityFormDialogProps(
      title = "Invalid Entity Test",
      onSubmit = _ => fail("onSubmit should not be called with invalid inputs"),
      onError = err => capturedError = Some(err),
      teams = TestTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      formSaveButton.fire()
    }

    capturedError shouldBe defined

  test("SaveEntityFormDialog displays visually the circle entity values passed"):
    val circleEntityToUpdate: MonadCoreEntity = MonadCoreEntityArranger.arrangeRedEntity(Circle, withOptionals = true)
    var submittedEntity: Option[MonadCoreEntity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = TestTeams,
      entityToUpdate = Some(circleEntityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("edit_circle_entity_form_dialog", rootNode, maxDiffPercentage = 10.0)
    }

  test("SaveEntityFormDialog displays architecturally the circle entity values passed"):
    val circleEntityToUpdate: MonadCoreEntity = MonadCoreEntityArranger.arrangeRedEntity(Circle, withOptionals = true)
    var submittedEntity: Option[MonadCoreEntity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = TestTeams,
      entityToUpdate = Some(circleEntityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("edit_circle_entity_form_dialog", activeStage)
    }

  test("SaveEntityFormDialog displays visually the rectangle entity values passed"):
    val rectangleEntityToUpdate: MonadCoreEntity = MonadCoreEntityArranger.arrangeRedEntity(Rectangle, withOptionals = true)
    var submittedEntity: Option[MonadCoreEntity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = TestTeams,
      entityToUpdate = Some(rectangleEntityToUpdate)
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("edit_rectangle_entity_form_dialog", rootNode, maxDiffPercentage = 10.0)
    }

  test("SaveEntityFormDialog displays architecturally the rectangle entity values passed"):
    val rectangleEntityToUpdate: MonadCoreEntity = MonadCoreEntityArranger.arrangeRedEntity(Rectangle, withOptionals = true)
    var submittedEntity: Option[MonadCoreEntity] = Option.empty

    val props = SaveEntityFormDialogProps(
      title = "Edit Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = TestTeams,
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
      teams = TestTeams
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
      teams = TestTeams
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
      teams = TestTeams
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
      teams = TestTeams
    )

    onFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectRectangleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesArchitecturalSnapshotOfStage("save_rectangle_entity_form_dialog_initial", activeStage)
    }