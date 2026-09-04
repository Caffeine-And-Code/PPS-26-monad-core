package monad_core.engine.model

/** Validated, non-empty identifier of a [[Team]]. */
opaque type TeamId = String

/** Creates and exposes validated [[TeamId]] values. */
object TeamId:

  /**
   * Creates a team identifier after trimming leading and trailing whitespace.
   *
   * @param teamId the identifier to validate
   * @return the trimmed identifier, or a [[TeamIdCannotBeEmpty]] error
   */
  def apply(teamId: String): Either[EngineError, TeamId] =
    Either.cond(teamId.trim.nonEmpty, teamId.trim, TeamIdCannotBeEmpty())

  /**
   * Validates an optional raw team identifier.
   *
   * @param optionalTeamId raw identifier, or `None` when no team is assigned
   * @return `Right(None)` when absent, `Right(Some(TeamId))` for a non-empty identifier, or
   *   [[TeamIdCannotBeEmpty]] for an empty identifier
   */
  def fromOption(optionalTeamId: Option[String]): Either[EngineError, Option[TeamId]] =
    ModelUtils.optionalize(optionalTeamId, TeamId(_))

  extension (teamId: TeamId)
    /**
     * Returns the underlying identifier.
     *
     * @return trimmed, non-empty identifier
     */
    def value: String = teamId
