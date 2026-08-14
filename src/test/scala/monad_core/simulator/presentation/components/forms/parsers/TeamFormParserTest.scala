package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.TeamId
import monad_core.simulator.MissingKeyInFormError
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class TeamFormParserTest extends AnyFunSuite with Inside with Matchers:
  val TeamIdValue: String   = "teamIdValue"
  val EnemyOneValue: String = "enemyOne"
  val EnemyTwoValue: String = "enemyTwo"

  def teamFormValues: Map[String, String] = Map(
    TeamFormParser.TeamIdKey  -> TeamIdValue,
    TeamFormParser.EnemiesKey -> s"$EnemyOneValue,$EnemyTwoValue"
  )

  test("A team can be converted from form values"):
    val expectedId      = TeamId(TeamIdValue).value
    val expectedEnemies = Set(TeamId(EnemyOneValue).value, TeamId(EnemyTwoValue).value)
    val formValues      = teamFormValues

    val parseResult = TeamFormParser.buildTeam(formValues)

    inside(parseResult):
      case Right(team) =>
        team.id should be(expectedId)
        team.enemies should be(expectedEnemies)

  test("If form values doesn't have the 'id' value the team cannot be parsed"):
    val expectedError           = MissingKeyInFormError(TeamFormParser.TeamIdKey)
    val formValuesWithMissingId = teamFormValues - TeamFormParser.TeamIdKey

    val parseResult = TeamFormParser.buildTeam(formValuesWithMissingId)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("If form values doesn't have the 'enemies' value the team cannot be parsed"):
    val expectedError                = MissingKeyInFormError(TeamFormParser.EnemiesKey)
    val formValuesWithMissingEnemies = teamFormValues - TeamFormParser.EnemiesKey

    val parseResult = TeamFormParser.buildTeam(formValuesWithMissingEnemies)

    inside(parseResult):
      case Left(error) =>
        error should be(expectedError)

  test("parseEnemies should return an empty set when raw string is empty"):
    val result = TeamFormParser.parseEnemies("")

    result.value should be(Set.empty[TeamId])

  test("parseEnemies should return an empty set when raw string is only whitespace/commas"):
    val cases = Table(
      "raw",
      "   ",
      ",,,",
      " , , "
    )

    forAll(cases): raw =>
      val result = TeamFormParser.parseEnemies(raw)

      result.value should be(Set.empty[TeamId])

  test("parseEnemies should trim tokens and ignore empty ones between separators"):
    val expected = Set(TeamId(EnemyOneValue).value, TeamId(EnemyTwoValue).value)

    val result = TeamFormParser.parseEnemies(s" $EnemyOneValue , ,$EnemyTwoValue ,")

    result.value should be(expected)

  test("parseEnemies should deduplicate repeated ids"):
    val expected = Set(TeamId(EnemyOneValue).value)

    val result = TeamFormParser.parseEnemies(s"$EnemyOneValue,$EnemyOneValue,$EnemyOneValue")

    result.value should be(expected)
