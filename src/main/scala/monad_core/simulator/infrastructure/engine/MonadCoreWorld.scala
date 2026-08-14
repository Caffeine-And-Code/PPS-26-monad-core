package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.Scene
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*
import monad_core.simulator.application.engine
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}

case class MonadCoreWorld(
    initialScene: Scene = Scene()
) extends World:

  var currentScene: Scene = initialScene

  override def getAllEntities: List[Entity] =
    currentScene.entities.values.toList

  override def getEntity(entityId: LocatableId): Either[EngineError, Entity] =
    currentScene.getEntity(entityId)

  override def createEntity(command: SaveEntityCommand): Either[EngineError, Unit] =
    for {
      scene <- currentScene.addEntity(command.entity)
    } yield currentScene = scene

  override def removeEntity(entityId: LocatableId): Either[EngineError, Unit] =
    for {
      entity <- currentScene.getEntity(entityId)
      scene  <- currentScene.removeEntity(entity)
    } yield currentScene = scene

  override def updateEntity(command: SaveEntityCommand): Either[EngineError, Unit] =
    for {
      entity <- currentScene.getEntity(command.entity.id)
      scene  <- currentScene.removeEntity(entity)
      scene  <- scene.addEntity(command.entity)
    } yield currentScene = scene

  override def getAllSurfaces: List[Surface] =
    currentScene.surfaces.values.toList

  override def getSurface(id: LocatableId): Either[EngineError, Surface] =
    currentScene.getSurface(id)

  override def createSurface(command: SaveSurfaceCommand): Either[EngineError, Unit] =
    for {
      scene <- currentScene.addSurface(command.surface)
    } yield currentScene = scene

  override def removeSurface(id: LocatableId): Either[EngineError, Unit] =
    for {
      surface <- currentScene.getSurface(id)
      scene   <- currentScene.removeSurface(surface)
    } yield currentScene = scene

  override def updateSurface(command: SaveSurfaceCommand): Either[EngineError, Unit] =
    for {
      surface <- currentScene.getSurface(command.surface.id)
      scene   <- currentScene.removeSurface(surface)
      scene   <- scene.addSurface(command.surface)
    } yield currentScene = scene

  override def getAllTeams: List[Team] =
    currentScene.teams.values.toList

  override def getTeam(id: TeamId): Either[EngineError, Team] =
    currentScene.getTeam(id)

  override def createTeam(command: SaveTeamCommand): Either[EngineError, Unit] =
    for {
      scene <- currentScene.addTeam(command.team)
    } yield currentScene = scene

  override def removeTeam(id: TeamId): Either[EngineError, Unit] =
    for {
      team  <- currentScene.getTeam(id)
      scene <- currentScene.removeTeam(team)
    } yield currentScene = scene

  override def updateTeam(command: SaveTeamCommand): Either[EngineError, Unit] =
    for {
      team  <- currentScene.getTeam(command.team.id)
      scene <- currentScene.removeTeam(team)
      scene <- scene.addTeam(command.team)
    } yield currentScene = scene

  override def scene: Scene = currentScene
