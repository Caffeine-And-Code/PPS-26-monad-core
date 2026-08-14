package monad_core.simulator.presentation.components.forms.parsers

import monad_core.simulator.MissingKeyInFormError
import monad_core.simulator.domain.engine.MonadCoreTeam
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.parsers.BaseFormParser.getValueSafe

object TeamFormParser:
  val TeamIdKey  = "id"
  val EnemiesKey = "enemies"

  def buildTeam(values: Map[String, String]): Either[BaseError, MonadCoreTeam] =
    for
      id      <- values.getValueSafe(TeamIdKey)
      _       <- if id.nonEmpty then Right(()) else Left(MissingKeyInFormError("teamId"))
      enemies <- values.getValueSafe(EnemiesKey)
    yield MonadCoreTeam(
      id = id,
      enemies = parseEnemies(enemies)
    )

  def buildUpdatedTeam(
      values: Map[String, String],
      teamToUpdate: MonadCoreTeam
  ): Either[BaseError, MonadCoreTeam] =
    for enemies <- values.getValueSafe(EnemiesKey)
    yield MonadCoreTeam(
      id = teamToUpdate.id,
      enemies = parseEnemies(enemies)
    )

  private[parsers] def parseEnemies(raw: String): Set[String] =
    raw
      .split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
      .toList
      .toSet
