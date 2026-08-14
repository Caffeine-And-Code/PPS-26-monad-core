package monad_core.simulator.infrastructure.engine.translators

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{TeamId, Team as EngineTeam}
import monad_core.simulator.domain.engine.MonadCoreTeam

object TeamTranslator:

  extension (team: EngineTeam)

    def toSimulationTeam: MonadCoreTeam =
      MonadCoreTeam(
        id = team.id.value,
        enemies = team.enemies.map(_.value)
      )

  extension (simulationTeam: MonadCoreTeam)

    def toEngineModel: Either[EngineError, EngineTeam] =
      for
        id <- TeamId(simulationTeam.id)
        enemies <- simulationTeam.enemies
          .foldLeft[Either[EngineError, Set[TeamId]]](Right(Set.empty)) { (previousSet, enemyId) =>
            for
              previousEnemies <- previousSet
              teamId          <- TeamId(enemyId)
            yield previousEnemies + teamId
          }

        team <- EngineTeam(id, enemies)
      yield team
