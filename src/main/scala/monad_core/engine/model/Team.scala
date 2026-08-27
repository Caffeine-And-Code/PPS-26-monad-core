package monad_core.engine.model

/** Immutable group of entities and the identifiers of its enemy teams. */
final case class Team private (
    id: TeamId,
    enemies: Set[TeamId]
)

object Team:

  /**
   * Creates a team from validated identifiers.
   *
   * @return
   *   an error when the team includes itself among its enemies
   */
  def apply(teamId: TeamId, enemies: Set[TeamId] = Set.empty): Either[EngineError, Team] =
    validate(new Team(teamId, enemies))

  /** Creates a team by validating its identifier and all enemy identifiers. */
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

    /** Adds an enemy, rejecting an invalid identifier or the team itself. */
    def addEnemy(teamId: String): Either[EngineError, Team] =
      TeamId(teamId).flatMap(tId => Team(team.id, team.enemies + tId))

    /** Removes an enemy; an invalid identifier leaves the team unchanged. */
    def removeEnemy(enemyId: String): Team =
      TeamId(enemyId).map(enemyTeamId => team.copy(enemies = team.enemies - enemyTeamId)) match
        case Left(_)            => team
        case Right(updatedTeam) => updatedTeam

    /** Tests whether a valid identifier belongs to this team's enemies. */
    def isEnemyOf(enemyId: String): Boolean =
      TeamId(enemyId).map(enemyTeamId => team.enemies.contains(enemyTeamId)) match
        case Left(_)      => false
        case Right(value) => value
