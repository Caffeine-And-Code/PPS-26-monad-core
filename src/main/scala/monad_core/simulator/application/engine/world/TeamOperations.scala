package monad_core.simulator.application.engine.world

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Team, TeamId}

case class SaveTeamCommand(
                            team: Team
                          )

private[world] trait TeamOperations:
  def getAllTeams: List[Team]

  def getTeam(id: TeamId): Either[EngineError, Team]

  def createTeam(command: SaveTeamCommand): Either[EngineError, World]

  def removeTeam(id: TeamId): Either[EngineError, World]

  def updateTeam(command: SaveTeamCommand): Either[EngineError, World]

