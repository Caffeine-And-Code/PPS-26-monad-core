package engine.model

case class Team(
               teamId: TeamId,
               enemies: Set[TeamId] = Set.empty
               )

object Team:

  def apply(teamId: TeamId, enemies: Set[TeamId] = Set.empty): Either[String, Team] =
    validate(new Team(teamId, enemies))

  private def validate(team: Team): Either[String, Team] =
    if team.enemies.contains(team.teamId) then
      Left("A team cannot be its own enemy")
    else
      Right(team)

  extension (t: Team)
    def addEnemy(teamId: TeamId): Either[String, Team] =
      Team(t.teamId, t.enemies + teamId)