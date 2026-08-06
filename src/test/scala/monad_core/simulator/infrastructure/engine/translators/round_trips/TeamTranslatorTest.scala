package monad_core.simulator.infrastructure.engine.translators.round_trips

import helpers.arrangers.MonadCoreTeamArranger
import monad_core.engine.model.Team
import monad_core.simulator.domain.engine.MonadCoreTeam
import monad_core.simulator.infrastructure.engine.translators.TeamTranslator.{toEngineModel, toSimulationTeam}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class TeamTranslatorTest extends AnyFunSuite with Matchers with Inside:

  test("TeamTranslator Round Trip property is respected for Engine Teams"):
    val possibleEntities = Table(
      "expectedTeam",
      Team.create("teamId").value,
      Team.create("teamId", Set("enemy1", "enemy2")).value,
    )

    forAll(possibleEntities): expectedTeam =>
      val translationResult = expectedTeam.toSimulationTeam.toEngineModel

      inside(translationResult):
        case Right(entity) =>
          entity should be(expectedTeam)

  test("SurfaceTranslator Round Trip property is respected for Simulator Surfaces"):
    val possibleEntities = Table(
      "expectedTeam",
      MonadCoreTeamArranger.arrangeRedTeamWithoutEnemies,
      MonadCoreTeam("teamId", enemies = Set("enemy1", "enemy2")),
    )

    forAll(possibleEntities): expectedTeam =>
      val translationResult = expectedTeam
        .toEngineModel
        .fold(
          error => fail(error.message),
          engineEntity => engineEntity.toSimulationTeam
        )

      translationResult should be(expectedTeam)