package integrations.monad_core.simulator.presentation.components.forms

import integrations.monad_core.simulator.presentation.support.{DialogTesting, FormTesting}
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*
import monad_core.simulator.TeamNotFoundDuringSelection
import monad_core.simulator.presentation.components.forms.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.*

class ChooseTeamFormDialogTest extends AnyFunSuite with Inside with Matchers with DialogTesting with FormTesting:
  val TeamComboBoxIndex: Int = 0

  private val teams: Seq[Team] = Seq(
    Team(TeamId("RedTeam").value, Set.empty).value,
    Team(TeamId("BlueTeam").value, Set.empty).value,
    Team(TeamId("GreenTeam").value, Set.empty).value
  )

  private def selectTeamInComboBox(teamIndex: Int): Unit =
    allFormComboBoxes(TeamComboBoxIndex).getSelectionModel.select(teamIndex)

  test("ChooseTeamFormDialog opens successfully"):
    val props = ChooseTeamFormDialogProps(
      teams = teams,
      onSubmit = _ => (),
      onError = _ => ()
    )

    runOnFxThread {
      val result = ChooseTeamFormDialog.show(props)
      inside(result):
        case Right(_) => ()
    }

  test("ChooseTeamFormDialog invokes onSubmit with the first team by default"):
    var submittedTeam: Option[Team] = None

    val props = ChooseTeamFormDialogProps(
      teams = teams,
      onSubmit = team => submittedTeam = Some(team),
      onError = err => fail(s"Unexpected error: $err")
    )

    runOnFxThread {
      getOrFail(ChooseTeamFormDialog.show(props))

      formSaveButton.fire()
    }

    submittedTeam should be(Some(teams.head))

  test("ChooseTeamFormDialog invokes onSubmit with the team selected by the user"):
    var submittedTeam: Option[Team] = None

    val props = ChooseTeamFormDialogProps(
      teams = teams,
      onSubmit = team => submittedTeam = Some(team),
      onError = err => fail(s"Unexpected error: $err")
    )

    runOnFxThread {
      getOrFail(ChooseTeamFormDialog.show(props))

      selectTeamInComboBox(2)

      formSaveButton.fire()
    }

    submittedTeam should be(Some(teams(2)))

  test("ChooseTeamFormDialog invokes onError when the teams list is empty"):
    var capturedError: Option[EngineError] = None

    val props = ChooseTeamFormDialogProps(
      teams = Seq.empty,
      onSubmit = _ => fail("onSubmit should not be called when there are no teams to choose from"),
      onError = err => capturedError = Some(err)
    )

    runOnFxThread {
      getOrFail(ChooseTeamFormDialog.show(props))

      formSaveButton.fire()
    }

    capturedError should be(Some(TeamNotFoundDuringSelection("")))

  test("ChooseTeamFormDialog matches visual snapshot"):
    val props = ChooseTeamFormDialogProps(
      teams = teams,
      onSubmit = _ => (),
      onError = _ => ()
    )

    runOnFxThread {
      getOrFail(ChooseTeamFormDialog.show(props))

      val activeStage = getRequiredActiveStage
      val rootNode: scalafx.scene.Node = activeStage.getScene.getRoot

      assertMatchesVisualSnapshot("choose_team_form_dialog_initial", rootNode, maxDiffPercentage = 9.2)
    }

  test("ChooseTeamFormDialog matches architectural snapshot"):
    val props = ChooseTeamFormDialogProps(
      teams = teams,
      onSubmit = _ => (),
      onError = _ => ()
    )

    runOnFxThread {
      getOrFail(ChooseTeamFormDialog.show(props))

      val activeStage = getRequiredActiveStage

      assertMatchesArchitecturalSnapshotOfStage("choose_team_form_dialog_initial", activeStage)
    }