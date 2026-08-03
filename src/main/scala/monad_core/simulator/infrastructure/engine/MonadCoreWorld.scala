package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.Scene
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*
import monad_core.simulator.application.engine
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, SaveTeamCommand, World}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.errors.ErrorsAdapter.adapt

case class MonadCoreWorld(
                           initialScene: Scene = Scene()
                         ) extends World:

  var currentScene: Scene = initialScene

  override def getAllEntities: List[Entity] =
    currentScene.entities.values.toList

  override def getEntity(entityId: LocatableId): Either[BaseError, Entity] =
    currentScene.getEntity(entityId).adapt()

  override def createEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for
      scene <- currentScene.addEntity(command.entity).adapt()
    yield currentScene = scene

  override def removeEntity(entityId: LocatableId): Either[BaseError, Unit] =
    for
      entity <- getEntity(entityId)
      scene <- currentScene.removeEntity(entity).adapt()
    yield currentScene = scene

  override def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for
      entityId = command.entity.id
      _ <- removeEntity(entityId)
      _ <- createEntity(command)
    yield currentScene

  override def getAllSurfaces: List[Surface] =
    currentScene.surfaces.values.toList

  override def getSurface(id: LocatableId): Either[BaseError, Surface] =
    currentScene.getSurface(id).adapt()

  override def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      scene <- currentScene.addSurface(command.surface).adapt()
    yield currentScene = scene

  override def removeSurface(id: LocatableId): Either[BaseError, Unit] =
    for
      surface <- getSurface(id)
      scene <- currentScene.removeSurface(surface).adapt()
    yield currentScene = scene

  override def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      surfaceId = command.surface.id
      _ <- removeSurface(surfaceId)
      _ <- createSurface(command)
    yield currentScene

  override def getAllTeams: List[Team] =
    currentScene.teams.values.toList

  override def getTeam(id: TeamId): Either[BaseError, Team] =
    currentScene.getTeam(id).adapt()

  override def createTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for
      scene <- currentScene.addTeam(command.team).adapt()
    yield currentScene = scene

  override def removeTeam(id: TeamId): Either[BaseError, Unit] =
    for
      team <- getTeam(id)
      scene <- currentScene.removeTeam(team).adapt()
    yield currentScene = scene

  override def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for {
      teamId = command.team.id
      _ <- removeTeam(teamId)
      _ <- createTeam(command)
    } yield currentScene

  override def scene: Scene = currentScene
