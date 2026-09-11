package monad_core.simulator.presentation.components.forms.parsers

import monad_core.engine.model.{Team, TeamId}
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe

/** Converts submitted team form values into validated engine teams. */
object TeamFormParser:

  /** Key of the team identifier. */
  val TeamIdKey = "id"

  /** Key of the comma-separated enemy identifiers. */
  val EnemiesKey = "enemies"

  /**
   * Builds a new team from its identifier and selected enemies.
   *
   * @param values
   *   submitted values containing `TeamIdKey` and `EnemiesKey`
   * @return
   *   the validated team, or the first missing-value or domain error
   */
  def buildTeam(values: Map[String, String]): Either[BaseError, Team] =
    for
      id     <- values.getValueSafe(TeamIdKey)
      teamId <- TeamId(id).adaptError()

      teamWithId <- Team.apply(teamId).adaptError()
      team       <- buildUpdatedTeam(values, teamWithId)
    yield team

  /**
   * Rebuilds a team with a new enemy set while preserving its identifier.
   *
   * @param values
   *   submitted values containing `EnemiesKey`
   * @param teamToUpdate
   *   team whose identifier is retained
   * @return
   *   the updated team, or the first missing-value or domain error
   */
  def buildUpdatedTeam(values: Map[String, String], teamToUpdate: Team): Either[BaseError, Team] =
    for
      enemies  <- values.getValueSafe(EnemiesKey)
      enemyIds <- parseEnemies(enemies)

      team <- Team.apply(teamToUpdate.id, enemyIds).adaptError()
    yield team

  /**
   * Parses comma-separated enemy identifiers, trimming whitespace and ignoring empty tokens.
   *
   * @param raw
   *   submitted enemy list
   * @return
   *   the distinct validated identifiers, or the first invalid identifier error
   */
  private[parsers] def parseEnemies(raw: String): Either[BaseError, Set[TeamId]] =
    val tokens = raw.split(",").map(_.trim).filter(_.nonEmpty).toList

    tokens.foldLeft(Right(Set.empty[TeamId]): Either[BaseError, Set[TeamId]]) {
      case (Right(acc), token) => TeamId(token).map(acc + _).adaptError()
      case (Left(err), _)      => Left(err)
    }
