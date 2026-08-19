package monad_core.simulator.application.engine.world

import monad_core.engine.model.Team
import monad_core.simulator.errors.BaseError

case class SaveTeamCommand(
    team: Team
)

private[world] trait TeamOperations:
  def getAllTeams: List[Team]

  def getTeam(id: String): Either[BaseError, Team]

  def createTeam(command: SaveTeamCommand): Either[BaseError, Unit]

  def removeTeam(id: String): Either[BaseError, Unit]

  def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit]
