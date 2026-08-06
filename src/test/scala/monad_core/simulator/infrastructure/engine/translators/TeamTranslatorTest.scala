package monad_core.simulator.infrastructure.engine.translators

import helpers.arrangers.MonadCoreTeamArranger
import helpers.arrangers.MonadCoreTeamArranger.{BlueTeamId, GreenTeamId, RedTeamId}
import monad_core.engine.model.{ATeamCannotBeItsOwnEnemy, Team}
import monad_core.simulator.domain.engine.MonadCoreTeam
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.translators.TeamTranslator.{toEngineModel, toSimulationTeam}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TeamTranslatorTest extends AnyFunSuite with Matchers with Inside:

  val enemies: Set[String] = Set(GreenTeamId, BlueTeamId)
  val engineTeamWithoutEnemies: Team = Team.create(RedTeamId).value
  val engineTeamWithEnemies: Team = Team.create(RedTeamId, enemies).value

  test("toSimulationTeam converts an engine team without any enemy to a simulation team correctly"):
    val engineTeam = Team.create(RedTeamId).value
    val expectedTranslation = MonadCoreTeamArranger.arrangeRedTeamWithoutEnemies

    val translationResult = engineTeam.toSimulationTeam

    translationResult should be(expectedTranslation)

  test("toSimulationTeam converts an engine team with enemies to a simulation team correctly"):
    val enemies: Set[String] = Set(GreenTeamId, BlueTeamId)
    val engineTeam = Team.create(RedTeamId, enemies).value
    val expectedTranslation = MonadCoreTeam(RedTeamId, enemies)

    val translationResult = engineTeam.toSimulationTeam

    translationResult should be(expectedTranslation)

  test("toEngineModel converts a valid simulation team without enemies to a engine team in a correct way"):
    val expectedEngineTeam = Team.create(RedTeamId).value
    val simulationTeam = MonadCoreTeamArranger.arrangeRedTeamWithoutEnemies

    val translationResult = simulationTeam.toEngineModel

    inside(translationResult):
      case Right(team) => team should be(expectedEngineTeam)

  test("toEngineModel converts a valid simulation team with enemies to a engine team in a correct way"):
    val expectedEngineTeam = Team.create(RedTeamId, enemies).value
    val simulationTeam = MonadCoreTeam(RedTeamId, enemies)

    val translationResult = simulationTeam.toEngineModel

    inside(translationResult):
      case Right(team) => team should be(expectedEngineTeam)

  test("toEngineModel propagates the error when a team is its own enemy"):
    val simulationTeam = MonadCoreTeam(id = RedTeamId, enemies = Set(RedTeamId))

    val translationResult = simulationTeam.toEngineModel

    inside(translationResult):
      case Left(error) => error should be(ATeamCannotBeItsOwnEnemy())