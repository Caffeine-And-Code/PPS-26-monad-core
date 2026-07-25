package integrations.monad_core.simulator.presentation.components.forms

import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Team, TeamId}
import monad_core.simulator.presentation.components.forms.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.*

class SaveEntityFormDialogTest extends AnyFunSuite with Inside with Matchers with MockFactory with DialogTesting with FormTesting:
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

  test("SaveEntityFormDialog opens successfully"):
    val props = SaveEntityFormDialogProps(
      title = "Create Entity",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    runOnFxThread {
      val result = SaveEntityFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveEntityFormDialog invokes onSubmit with constructed Entity on valid input"):
    var submittedEntity: Option[Entity] = None
    val RadiusFieldIndex = 2
    val SpeedFieldIndex = 3
    val WeightFieldIndex = 4
    val HealthFieldIndex = 5
    val props = SaveEntityFormDialogProps(
      title = "Add Entity Test",
      onSubmit = entity => submittedEntity = Some(entity),
      onError = err => fail(s"Unexpected error: $err"),
      teams = testTeams
    )

    runOnFxThread {
      getOrFail(SaveEntityFormDialog.show(props))


      allFormFields(RadiusFieldIndex).setText("10.0")
      allFormFields(SpeedFieldIndex).setText("5.0")
      allFormFields(WeightFieldIndex).setText("70.0")
      allFormFields(HealthFieldIndex).setText("100.0")

      selectCircleInComboBox()

      formSaveButton.fire()
    }

    submittedEntity shouldBe defined

  test("SaveEntityFormDialog invokes onError when form values are invalid"):
    var capturedError: Option[EngineError] = None

    val props = SaveEntityFormDialogProps(
      title = "Invalid Entity Test",
      onSubmit = _ => fail("onSubmit should not be called with invalid inputs"),
      onError = err => capturedError = Some(err),
      teams = testTeams
    )

    runOnFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      formSaveButton.fire()
    }

    capturedError shouldBe defined

  test("SaveEntityFormDialog Circle matches visual snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    runOnFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectCircleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("save_circle_entity_form_dialog_initial", rootNode, maxDiffPercentage = 0.2)
    }


  test("SaveEntityFormDialog Circle matches architectural snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    runOnFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectCircleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesSnapshotOfStage("save_circle_entity_form_dialog_initial", activeStage)
    }

  test("SaveEntityFormDialog Rectangle matches visual snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    runOnFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectRectangleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("save_rectangle_entity_form_dialog_initial", rootNode, maxDiffPercentage = 0.2)
    }

  test("SaveEntityFormDialog Rectangle matches architectural snapshot"):
    val props = SaveEntityFormDialogProps(
      title = "Visual Save Entity Test",
      onSubmit = _ => (),
      onError = _ => (),
      teams = testTeams
    )

    runOnFxThread {
      getOrFail(SaveEntityFormDialog.show(props))

      selectRectangleInComboBox()

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesSnapshotOfStage("save_rectangle_entity_form_dialog_initial", activeStage)
    }