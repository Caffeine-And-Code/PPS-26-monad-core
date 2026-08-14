package integrations.monad_core.simulator.presentation.components.forms

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import monad_core.engine.model.*
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.*

class SaveTeamFormDialogTest
    extends AnyFunSuite
    with Inside
    with Matchers
    with DialogTesting
    with FormTesting:
  val TeamNameFieldIndex: Int      = 0
  val EnemiesMultiSelectIndex: Int = 0

  private val possibleEnemies: Seq[Team] = Seq(
    Team(TeamId("RedTeam").value, Set.empty).value,
    Team(TeamId("BlueTeam").value, Set.empty).value,
    Team(TeamId("GreenTeam").value, Set.empty).value
  )

  private def selectEnemyInMultiSelect(enemyIndex: Int): Unit =
    allFormMultiSelects(EnemiesMultiSelectIndex).selectionModel.value.select(enemyIndex)

  test("SaveTeamFormDialog opens successfully"):
    val props = SaveTeamFormDialogProps(
      title = "Create Team",
      onSubmit = _ => (),
      onError = _ => (),
      possibleEnemies = possibleEnemies
    )

    onFxThread {
      val result = SaveTeamFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveTeamFormDialog opens successfully by providing a team to edit"):
    val teamToUpdate = possibleEnemies.head

    val props = SaveTeamFormDialogProps(
      title = "Edit Team",
      onSubmit = _ => (),
      onError = _ => (),
      possibleEnemies = possibleEnemies,
      teamToUpdate = Some(teamToUpdate)
    )

    onFxThread {
      val result = SaveTeamFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("SaveTeamFormDialog invokes onSubmit with constructed Team on valid input"):
    var submittedTeam: Option[Team] = None

    val props = SaveTeamFormDialogProps(
      title = "Add Team Test",
      onSubmit = team => submittedTeam = Some(team),
      onError = err => fail(s"Unexpected error: $err"),
      possibleEnemies = possibleEnemies
    )

    onFxThread {
      getOrFail(SaveTeamFormDialog.show(props))

      allFormFields(TeamNameFieldIndex).setText("NewTeam")
      selectEnemyInMultiSelect(0)

      formSaveButton.fire()
    }

    submittedTeam shouldBe defined

  test("SaveTeamFormDialog invokes onSubmit with correct team name and enemies"):
    var submittedTeam: Option[Team] = None
    val expectedName                = "NewTeam"

    val props = SaveTeamFormDialogProps(
      title = "Add Team Test",
      onSubmit = team => submittedTeam = Some(team),
      onError = err => fail(s"Unexpected error: $err"),
      possibleEnemies = possibleEnemies
    )

    onFxThread {
      getOrFail(SaveTeamFormDialog.show(props))

      allFormFields(TeamNameFieldIndex).setText(expectedName)
      selectEnemyInMultiSelect(0)
      selectEnemyInMultiSelect(1)

      formSaveButton.fire()
    }

    submittedTeam shouldBe defined
    val providedTeam = submittedTeam.get
    providedTeam.id should be(TeamId(expectedName).value)
    providedTeam.enemies should be(Set(possibleEnemies.head.id, possibleEnemies(1).id))

  test("SaveTeamFormDialog invokes onError when form values are invalid"):
    var capturedError: Option[BaseError] = None

    val props = SaveTeamFormDialogProps(
      title = "Invalid Team Test",
      onSubmit = _ => fail("onSubmit should not be called with invalid inputs"),
      onError = err => capturedError = Some(err),
      possibleEnemies = possibleEnemies
    )

    onFxThread {
      getOrFail(SaveTeamFormDialog.show(props))

      formSaveButton.fire()
    }

    capturedError shouldBe defined

  test("SaveTeamFormDialog displays visually the team values passed for editing"):
    val teamToUpdate = possibleEnemies.head

    val props = SaveTeamFormDialogProps(
      title = "Edit Team Test",
      onSubmit = _ => (),
      onError = err => fail(s"Unexpected error: $err"),
      possibleEnemies = possibleEnemies,
      teamToUpdate = Some(teamToUpdate)
    )

    onFxThread {
      getOrFail(SaveTeamFormDialog.show(props))

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("edit_team_form_dialog", rootNode, maxDiffPercentage = 10.0)
    }

  test("SaveTeamFormDialog displays architecturally the team values passed for editing"):
    val teamToUpdate = possibleEnemies.head

    val props = SaveTeamFormDialogProps(
      title = "Edit Team Test",
      onSubmit = _ => (),
      onError = err => fail(s"Unexpected error: $err"),
      possibleEnemies = possibleEnemies,
      teamToUpdate = Some(teamToUpdate)
    )

    onFxThread {
      getOrFail(SaveTeamFormDialog.show(props))

      val activeStage = getRequiredActiveStage

      assertMatchesArchitecturalSnapshotOfStage("edit_team_form_dialog", activeStage)
    }

  test("SaveTeamFormDialog matches visual snapshot on creation"):
    val props = SaveTeamFormDialogProps(
      title = "Visual Save Team Test",
      onSubmit = _ => (),
      onError = _ => (),
      possibleEnemies = possibleEnemies
    )

    onFxThread {
      getOrFail(SaveTeamFormDialog.show(props))

      val activeStage                  = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot(
        "save_team_form_dialog_initial",
        rootNode,
        maxDiffPercentage = 9.2
      )
    }

  test("SaveTeamFormDialog matches architectural snapshot on creation"):
    val props = SaveTeamFormDialogProps(
      title = "Visual Save Team Test",
      onSubmit = _ => (),
      onError = _ => (),
      possibleEnemies = possibleEnemies
    )

    onFxThread {
      getOrFail(SaveTeamFormDialog.show(props))

      val activeStage = getRequiredActiveStage

      assertMatchesArchitecturalSnapshotOfStage("save_team_form_dialog_initial", activeStage)
    }
