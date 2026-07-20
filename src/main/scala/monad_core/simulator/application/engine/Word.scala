package monad_core.simulator.application.engine

import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, LocatableId, Surface, Team, TeamId}

case class SaveEntityCommand(
                              entity: Entity
                              )

private trait EntityServices:
  def getAllEntities: List[Entity]
  def getEntity(entityId: LocatableId): Either[EngineError, Entity]
  def createEntity(command: SaveEntityCommand): Either[EngineError, State]
  def removeEntity(entityId: LocatableId): Either[EngineError, State]
  def updateEntity(command: SaveEntityCommand): Either[EngineError, State]


case class SaveSurfaceCommand(
                                surface: Surface
                              )

private trait SurfaceServices:
  def getAllSurfaces: List[Surface]
  def getSurface(id: LocatableId): Either[EngineError, Surface]
  def createSurface(command: SaveSurfaceCommand): Either[EngineError, State]
  def removeSurface(id: LocatableId): Either[EngineError, State]
  def updateSurface(command: SaveSurfaceCommand): Either[EngineError, State]

case class SaveTeamCommand(
                                 team: Team
                               )

private trait TeamServices:
  def getAllTeams: List[Team]
  def getTeam(id: TeamId): Either[EngineError, Team]
  def createTeam(command: SaveTeamCommand): Either[EngineError, State]
  def removeTeam(id: TeamId): Either[EngineError, State]
  def updateTeam(command: SaveTeamCommand): Either[EngineError, State]

trait Word extends TeamServices, SurfaceServices, EntityServices