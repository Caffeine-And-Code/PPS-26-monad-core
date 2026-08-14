package monad_core.simulator.presentation.components.forms

import helpers.arrangers.MonadCoreTeamArranger
import monad_core.engine.model.*
import monad_core.simulator.domain.engine.MonadCoreTeam
import monad_core.simulator.presentation.components.forms.base.{MultiSelectFieldSpec, TextFieldSpec}
import monad_core.simulator.presentation.components.forms.parsers.TeamFormParser
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SaveTeamFormDialogTest extends AnyFunSuite with Inside with Matchers:

  private val RedTeamId   = MonadCoreTeamArranger.RedTeamId
  private val BlueTeamId  = MonadCoreTeamArranger.BlueTeamId
  private val GreenTeamId = MonadCoreTeamArranger.GreenTeamId

  private val possibleEnemies: Seq[MonadCoreTeam] = MonadCoreTeamArranger.arrangeTeams

  test("buildDefaultValues should return empty defaults when no team is provided"):
    val result = SaveTeamFormDialog.buildDefaultValues(None)

    result should be(SaveTeamFormDefaultValues())

  test("buildDefaultValues should map a team's id and empty enemies"):
    val team = MonadCoreTeam(RedTeamId, Set.empty)

    val result = SaveTeamFormDialog.buildDefaultValues(Some(team))

    result.teamName should be(Some(RedTeamId))
    result.enemies should be(Seq.empty)

  test("buildDefaultValues should map a team's enemies"):
    val team = MonadCoreTeam(RedTeamId, Set(BlueTeamId, GreenTeamId))

    val result = SaveTeamFormDialog.buildDefaultValues(Some(team))

    result.teamName should be(Some(RedTeamId))
    result.enemies should contain theSameElementsAs Seq(BlueTeamId, GreenTeamId)

  test("buildTeamCreationFields should build the name field followed by the enemies field"):
    val record = BuildSaveTeamFormFieldsRecord(possibleEnemies, SaveTeamFormDefaultValues())

    val fields = SaveTeamFormDialog.buildTeamCreationFields(record)

    fields.map(_.id) should be(
      Seq(TeamFormParser.TeamIdKey, TeamFormParser.EnemiesKey)
    )

  test("buildTeamCreationFields should propagate the default team name into the name field"):
    val defaultValues = SaveTeamFormDefaultValues(teamName = Some("NewTeam"))
    val record        = BuildSaveTeamFormFieldsRecord(possibleEnemies, defaultValues)

    val fields = SaveTeamFormDialog.buildTeamCreationFields(record)

    inside(fields.find(_.id == TeamFormParser.TeamIdKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("NewTeam"))

  test(
    "buildTeamCreationFields should build the enemies field from possible enemies and default enemies"
  ):
    val defaultValues = SaveTeamFormDefaultValues(enemies = Seq(BlueTeamId))
    val record        = BuildSaveTeamFormFieldsRecord(possibleEnemies, defaultValues)

    val fields = SaveTeamFormDialog.buildTeamCreationFields(record)

    inside(fields.find(_.id == TeamFormParser.EnemiesKey).value):
      case multi: MultiSelectFieldSpec =>
        multi.options should be(possibleEnemies.map(_.id))
        multi.defaultValues should be(Seq(BlueTeamId))

  test(
    "buildTeamCreationFields should return an empty options list when no possible enemies are provided"
  ):
    val record = BuildSaveTeamFormFieldsRecord(Seq.empty, SaveTeamFormDefaultValues())

    val fields = SaveTeamFormDialog.buildTeamCreationFields(record)

    inside(fields.find(_.id == TeamFormParser.EnemiesKey).value):
      case multi: MultiSelectFieldSpec => multi.options should be(Seq.empty)

  test("buildTeamEditFields should only build the enemies field"):
    val record = BuildSaveTeamFormFieldsRecord(possibleEnemies, SaveTeamFormDefaultValues())

    val fields = SaveTeamFormDialog.buildTeamEditFields(record)

    fields.map(_.id) should be(Seq(TeamFormParser.EnemiesKey))

  test(
    "buildTeamEditFields should build the enemies field from possible enemies and default enemies"
  ):
    val defaultValues = SaveTeamFormDefaultValues(enemies = Seq(GreenTeamId))
    val record        = BuildSaveTeamFormFieldsRecord(possibleEnemies, defaultValues)

    val fields = SaveTeamFormDialog.buildTeamEditFields(record)

    inside(fields.find(_.id == TeamFormParser.EnemiesKey).value):
      case multi: MultiSelectFieldSpec =>
        multi.options should be(possibleEnemies.map(_.id))
        multi.defaultValues should be(Seq(GreenTeamId))

  test(
    "buildTeamEditFields should return an empty options list when no possible enemies are provided"
  ):
    val record = BuildSaveTeamFormFieldsRecord(Seq.empty, SaveTeamFormDefaultValues())

    val fields = SaveTeamFormDialog.buildTeamEditFields(record)

    inside(fields.find(_.id == TeamFormParser.EnemiesKey).value):
      case multi: MultiSelectFieldSpec => multi.options should be(Seq.empty)
