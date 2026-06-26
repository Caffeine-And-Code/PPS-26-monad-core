package engine.model

opaque type TeamId = String

object TeamId:

  def apply(teamId: String): Either[String, TeamId] =
    Either.cond(teamId.trim.nonEmpty, teamId.trim, "TeamId cannot be empty")

  extension (teamId: TeamId)

    def value: String = teamId