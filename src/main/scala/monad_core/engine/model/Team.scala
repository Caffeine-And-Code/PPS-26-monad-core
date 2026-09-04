package monad_core.engine.model

/**
 * Immutable group identified by a [[TeamId]] and associated with its enemy teams.
 *
 * @param id validated team identifier
 * @param enemies identifiers treated as enemies by this team
 */
final case class Team private (
    id: TeamId,
    enemies: Set[TeamId]
)

/** Validated constructors and immutable update operations for [[Team]]. */
object Team:

  /**
   * Creates a team from validated identifiers.
   *
   * @param teamId validated identifier of the new team
   * @param enemies validated enemy identifiers
   * @return the team, or [[ATeamCannotBeItsOwnEnemy]] when `enemies` contains `teamId`
   */
  def apply(teamId: TeamId, enemies: Set[TeamId] = Set.empty): Either[EngineError, Team] =
    validate(new Team(teamId, enemies))

  /**
   * Creates a team by validating its identifier and all enemy identifiers.
   *
   * @param teamId raw non-empty team identifier
   * @param enemies raw enemy identifiers
   * @return the team, or the first identifier or a validation error
   */
  def create(teamId: String, enemies: Set[String] = Set.empty): Either[EngineError, Team] =
    val enemiesTeamId: Either[EngineError, Set[TeamId]] =
      enemies.foldLeft(Right(Set.empty[TeamId]): Either[EngineError, Set[TeamId]]) {
        case (Right(acc), nextString) =>
          TeamId(nextString).map(teamId => acc + teamId)
        case (Left(error), _) =>
          Left(error)
      }

    for {
      validEnemies <- enemiesTeamId
      validTeamId  <- TeamId(teamId)
      team         <- validate(new Team(validTeamId, validEnemies))
    } yield team

  private def validate(team: Team): Either[EngineError, Team] =
    if team.enemies.contains(team.id) then Left(ATeamCannotBeItsOwnEnemy())
    else Right(team)

  extension (team: Team)

    /**
     * Adds a validated enemy identifier.
     *
     * @param teamId raw identifier of the enemy to add
     * @return the updated team, [[TeamIdCannotBeEmpty]] for an empty identifier, or [[ATeamCannotBeItsOwnEnemy]] when
     *   the identifier equals this team's identifier
     */
    def addEnemy(teamId: String): Either[EngineError, Team] =
      TeamId(teamId).flatMap(tId => Team(team.id, team.enemies + tId))

    /**
     * Removes an enemy identifier when it is valid and present.
     *
     * @param enemyId raw identifier to remove
     * @return the updated team, or this team unchanged when the identifier is invalid or absent
     */
    def removeEnemy(enemyId: String): Team =
      TeamId(enemyId).map(enemyTeamId => team.copy(enemies = team.enemies - enemyTeamId)) match
        case Left(_)            => team
        case Right(updatedTeam) => updatedTeam

    /**
     * Tests whether an identifier belongs to this team's enemies.
     *
     * @param enemyId raw identifier to test
     * @return `true` when the identifier is valid and present in `enemies`; `false` otherwise
     */
    def isEnemyOf(enemyId: String): Boolean =
      TeamId(enemyId).map(enemyTeamId => team.enemies.contains(enemyTeamId)) match
        case Left(_)      => false
        case Right(value) => value
