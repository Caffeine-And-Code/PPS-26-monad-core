package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Team, TeamId}
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe

object TeamFormParser :
  val TeamIdKey = "id"
  val EnemiesKey = "enemies"

  def buildTeam(values: Map[String, String]) : Either[EngineError, Team] =
    for
      id <- values.getValueSafe(TeamIdKey)
      teamId <- TeamId(id)

      enemies <- values.getValueSafe(EnemiesKey)
      enemyIds <- parseEnemies(enemies)

      team <- Team.apply(teamId, enemyIds)
    yield team

  private[parsers] def parseEnemies(raw: String): Either[EngineError, Set[TeamId]] =
    val tokens = raw.split(",").map(_.trim).filter(_.nonEmpty).toList

    tokens.foldLeft(Right(Set.empty[TeamId]): Either[EngineError, Set[TeamId]]) {
      case (Right(acc), token) => TeamId(token).map(acc + _)
      case (Left(err), _) => Left(err)
    }
