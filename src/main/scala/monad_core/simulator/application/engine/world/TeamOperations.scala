package monad_core.simulator.application.engine.world

import monad_core.engine.model.Team
import monad_core.simulator.errors.BaseError

/**
 * Application command carrying a team to be created or updated.
 *
 * @param team team supplied to the world operation
 */
case class SaveTeamCommand(
    team: Team
)

/**
 * Contract for querying and editing the teams contained in a world.
 *
 * Mutation errors from the engine domain are exposed as [[BaseError]], and mutations
 * will be rejected while the world is in simulation mode.
 */
private[world] trait TeamOperations:

  /** @return all teams currently contained in the world, with no ordering guarantee */
  def getAllTeams: List[Team]

  /**
   * Retrieves a team from its external string identifier.
   *
   * @param id raw team identifier
   * @return the matching team, or a validation/not-found error
   */
  def getTeam(id: String): Either[BaseError, Team]

  /**
   * Adds a team to the world.
   *
   * @param command command containing the team to add
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the edit is invalid or not allowed
   */
  def createTeam(command: SaveTeamCommand): Either[BaseError, Unit]

  /**
   * Removes a team using its external string identifier.
   *
   * @param id raw identifier of the team to remove
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the identifier is invalid,
   *         the team is missing, or the edit is not allowed
   */
  def removeTeam(id: String): Either[BaseError, Unit]

  /**
   * Replaces the team having the same identifier as the command team.
   *
   * @param command command containing the updated team
   * @return `Right(Unit)` on success, or `Left(BaseError)` when the team is missing,
   *         the update is invalid, or the edit is not allowed
   */
  def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit]
