package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.Scene
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, LocatableId, Surface, Team, TeamId}
import monad_core.simulator.application.engine.{SaveEntityCommand, SaveSurfaceCommand, SaveTeamCommand, Word}

case class EngineWord(
  scene: Scene = Scene()
) extends Word:
  override def getAllEntities: List[Entity] =
    scene.entities.values.toList

  override def getEntity(entityId: LocatableId): Either[EngineError, Entity] =
    scene.getEntity(entityId)

  override def createEntity(command: SaveEntityCommand): Either[EngineError, State] =
    scene.addEntity(command.entity)

  override def removeEntity(entityId: LocatableId): Either[EngineError, State] =
    scene.getEntity(entityId).flatMap(scene.removeEntity)

  override def updateEntity(command: SaveEntityCommand): Either[EngineError, State] =
    scene.getEntity(command.entity.id)
      .flatMap(scene.removeEntity)
      .flatMap(_.addEntity(command.entity))

  override def getAllSurfaces: List[Surface] =
    scene.surfaces.values.toList

  override def getSurface(id: LocatableId): Either[EngineError, Surface] =
    scene.getSurface(id)

  override def createSurface(command: SaveSurfaceCommand): Either[EngineError, State] =
    scene.addSurface(command.surface)

  override def removeSurface(id: LocatableId): Either[EngineError, State] =
    scene.getSurface(id).flatMap(scene.removeSurface)

  override def updateSurface(command: SaveSurfaceCommand): Either[EngineError, State] =
    scene.getSurface(command.surface.id)
      .flatMap(scene.removeSurface)
      .flatMap(_.addSurface(command.surface))

  override def getAllTeams: List[Team] =
    scene.teams.values.toList

  override def getTeam(id: TeamId): Either[EngineError, Team] =
    scene.getTeam(id)

  override def createTeam(command: SaveTeamCommand): Either[EngineError, State] =
    scene.addTeam(command.team)

  override def removeTeam(id: TeamId): Either[EngineError, State] =
    scene.getTeam(id).flatMap(scene.removeTeam)

  override def updateTeam(command: SaveTeamCommand): Either[EngineError, State] =
    scene.getTeam(command.team.id)
      .flatMap(scene.removeTeam)
      .flatMap(_.addTeam(command.team))
