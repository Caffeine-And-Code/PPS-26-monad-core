package monad_core.engine.model

/** Validated, non-empty identifier of a [[Team]]. */
opaque type TeamId = String

object TeamId:

  /**
   * Creates a team identifier after trimming leading and trailing whitespace.
   *
   * @param teamId
   *   the identifier to validate
   * @return
   *   the trimmed identifier, or a [[TeamIdCannotBeEmpty]] error
   */
  def apply(teamId: String): Either[EngineError, TeamId] =
    Either.cond(teamId.trim.nonEmpty, teamId.trim, TeamIdCannotBeEmpty())

  def fromOption(optionalTeamId: Option[String]): Either[EngineError, Option[TeamId]] =
    ModelUtils.optionalize(optionalTeamId, TeamId(_))

  extension (teamId: TeamId)
    /** Returns the underlying identifier. */
    def value: String = teamId
