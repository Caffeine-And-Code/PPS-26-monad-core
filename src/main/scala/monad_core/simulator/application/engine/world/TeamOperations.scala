package monad_core.simulator.application.engine.world

import monad_core.simulator.domain.engine.MonadCoreTeam
import monad_core.simulator.errors.BaseError

case class SaveTeamCommand(
                            team: MonadCoreTeam
                          )

private[world] trait TeamOperations:
  def getAllTeams: Either[BaseError, List[MonadCoreTeam]]

  def getTeam(id: String): Either[BaseError, MonadCoreTeam]

  def createTeam(command: SaveTeamCommand): Either[BaseError, Unit]

  def removeTeam(id: String): Either[BaseError, Unit]

  def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit]

