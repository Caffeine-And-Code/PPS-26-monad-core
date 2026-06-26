package engine.model

import engine.errors.EngineError

opaque type TeamId = String

object TeamId:

  def apply(teamId: String): Either[EngineError, TeamId] =
    Either.cond(teamId.trim.nonEmpty, teamId.trim, TeamIdCannotBeEmpty())

  extension (teamId: TeamId)

    def value: String = teamId