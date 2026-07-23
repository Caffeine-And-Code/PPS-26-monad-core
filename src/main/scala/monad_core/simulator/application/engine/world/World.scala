package monad_core.simulator.application.engine.world

import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*

trait World extends TeamOperations, EntityOperations, SurfaceOperations:
  //TODO: The return type should be a domain entity, which will be a mapper to the State Entity 
  def snapshot: State

private final case class WorldImpl(snapshot: State) extends World:

  export snapshot.{allEntities, allTeams, allSurfaces, getEntity, getTeam, getSurface}

  def createEntity(command: SaveEntityCommand): Either[EngineError, World] =
    snapshot.addEntity(command.entity).map(WorldImpl.apply)

  def removeEntity(entityId: LocatableId): Either[EngineError, World] =
    snapshot.getEntity(entityId).flatMap(snapshot.removeEntity).map(WorldImpl.apply)

  def updateEntity(command: SaveEntityCommand): Either[EngineError, World] =
    removeEntity(command.entity.id).flatMap(_ => createEntity(command))

  def createSurface(command: SaveSurfaceCommand): Either[EngineError, World] =
    snapshot.addSurface(command.surface).map(WorldImpl.apply)

  def removeSurface(id: LocatableId): Either[EngineError, World] =
    snapshot.getSurface(id).flatMap(snapshot.removeSurface).map(WorldImpl.apply)

  def updateSurface(command: SaveSurfaceCommand): Either[EngineError, World] =
    removeSurface(command.surface.id).flatMap(_ => createSurface(command))

  def createTeam(command: SaveTeamCommand): Either[EngineError, World] =
    snapshot.addTeam(command.team).map(WorldImpl.apply)

  def removeTeam(id: TeamId): Either[EngineError, World] =
    snapshot.getTeam(id).flatMap(snapshot.removeTeam).map(WorldImpl.apply)

  def updateTeam(command: SaveTeamCommand): Either[EngineError, World] =
    removeTeam(command.team.id).flatMap(_ => createTeam(command))

  override def getAllEntities: List[Entity] = snapshot.allEntities

  override def getAllSurfaces: List[Surface] = snapshot.allSurfaces

  override def getAllTeams: List[Team] = snapshot.allTeams

object World:
  def apply(state: State): World = WorldImpl(state)