package engine.model

case class Team private(
               teamId: TeamId,
               enemies: Set[TeamId] = Set.empty
               )

object Team:

  def apply(teamId: TeamId, enemies: Set[TeamId] = Set.empty): Either[String, Team] =
    validate(new Team(teamId, enemies))

  def create(teamId: String, enemies: Set[String] = Set.empty): Either[String, Team] = {
    val enemiesTeamId: Either[String, Set[TeamId]] =
      enemies.foldLeft(Right(Set.empty[TeamId]): Either[String, Set[TeamId]]) {
        case (Right(acc), nextString) =>
          TeamId(nextString).map(teamId => acc + teamId)
        case (Left(error), _) =>
          Left(error)
      }

    for {
      validEnemies <- enemiesTeamId
      validTeamId <- TeamId(teamId)
      team <- validate(new Team(validTeamId, validEnemies))
    } yield team
  }

  private def validate(team: Team): Either[String, Team] =
    if team.enemies.contains(team.teamId) then
      Left("A team cannot be its own enemy")
    else
      Right(team)

  extension (t: Team)
    def addEnemy(teamId: String): Either[String, Team] =
      TeamId(teamId).flatMap(tId => Team(t.teamId, t.enemies + tId))

    def removeEnemy(enemyId: String): Team = 
      TeamId(enemyId).map(enemyTeamId => t.copy(enemies = t.enemies - enemyTeamId)) match {
        case Left(_) => t
        case Right(team) => team
      }
    
    
    def isEnemyOf(enemyId: String): Boolean =
      TeamId(enemyId).map(enemyTeamId => t.enemies.contains(enemyTeamId)) match {
        case Left(_) => false
        case Right(value) => value
      }