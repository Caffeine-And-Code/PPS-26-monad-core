package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.*
import monad_core.simulator.presentation.components.forms.base.SelectFieldSpec
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChooseTeamFormDialogTest extends AnyFunSuite with Inside with Matchers:

  private val teams: Seq[Team] = Seq(
    Team(TeamId("RedTeam").value, Set.empty).value,
    Team(TeamId("BlueTeam").value, Set.empty).value,
    Team(TeamId("GreenTeam").value, Set.empty).value
  )

  test("buildFields should build a single select field with the correct id and label"):
    val fields = ChooseTeamFormDialog.buildSelect(teams)

    fields.map(_.id) should be(Seq(ChooseTeamFormDialog.TeamKey))

    inside(fields.head):
      case select: SelectFieldSpec =>
        select.label should be("Team")

  test("buildFields should build options from the provided teams"):
    val fields = ChooseTeamFormDialog.buildSelect(teams)

    inside(fields.head):
      case select: SelectFieldSpec =>
        select.options should be(teams.map(_.id.value))

  test("buildFields should return an empty options list when no teams are provided"):
    val fields = ChooseTeamFormDialog.buildSelect(Seq.empty)

    inside(fields.head):
      case select: SelectFieldSpec => select.options should be(Seq.empty)

  test(
    "buildFields should not set a default value, letting the underlying control pick the first option"
  ):
    val fields = ChooseTeamFormDialog.buildSelect(teams)

    inside(fields.head):
      case select: SelectFieldSpec => select.defaultValue should be(None)

  test("buildFields should have no dependent fields"):
    val fields = ChooseTeamFormDialog.buildSelect(teams)

    inside(fields.head):
      case select: SelectFieldSpec => select.dependentFields should be(Map.empty)
