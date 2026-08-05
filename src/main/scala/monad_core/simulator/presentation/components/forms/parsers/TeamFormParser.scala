package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Team, TeamId}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe

object TeamFormParser:
  val TeamIdKey = "id"
  val EnemiesKey = "enemies"

  def buildTeam(values: Map[String, String]): Either[BaseError, Team] =
    for
      id <- values.getValueSafe(TeamIdKey)
      teamId <- TeamId(id).adaptError()

      teamWithId <- Team.apply(teamId).adaptError()
      team <- buildUpdatedTeam(values, teamWithId)
    yield team

  def buildUpdatedTeam(values: Map[String, String], teamToUpdate: Team): Either[BaseError, Team] =
    for
      enemies <- values.getValueSafe(EnemiesKey)
      enemyIds <- parseEnemies(enemies)

      team <- Team.apply(teamToUpdate.id, enemyIds).adaptError()
    yield team

  private[parsers] def parseEnemies(raw: String): Either[BaseError, Set[TeamId]] =
    val tokens = raw.split(",").map(_.trim).filter(_.nonEmpty).toList

    tokens.foldLeft(Right(Set.empty[TeamId]): Either[BaseError, Set[TeamId]]) {
      case (Right(acc), token) => TeamId(token).adaptError().map(acc + _)
      case (Left(err), _) => Left(err)
    }
