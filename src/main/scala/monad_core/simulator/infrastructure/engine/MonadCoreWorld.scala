package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.Scene
import monad_core.engine.model.*
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, SaveTeamCommand, World}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreSurface, MonadCoreTeam}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.infrastructure.engine.translators.BaseTranslator.{toSimulationEitherEntity, toSimulationEitherSurface, toSimulationEitherTeam}
import monad_core.simulator.infrastructure.engine.translators.EntityTranslator.{toEngineModel, toSimulationEntity}
import monad_core.simulator.infrastructure.engine.translators.SurfaceTranslator.{toEngineModel, toSimulationSurface}
import monad_core.simulator.infrastructure.engine.translators.TeamTranslator.{toEngineModel, toSimulationTeam}

case class MonadCoreWorld(
                           initialScene: Scene = Scene()
                         ) extends World:

  var currentScene: Scene = initialScene

  override def getAllEntities: List[MonadCoreEntity] =
    currentScene.entities.values.toList.map(_.toSimulationEntity)

  override def getEntity(entityId: String): Either[BaseError, MonadCoreEntity] =
    for
      id <- LocatableId(entityId).adaptError()
      entity <- currentScene.getEntity(id).toSimulationEitherEntity.adaptError()
    yield entity

  override def createEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for
      entityToAdd <- command.entity.toEngineModel.adaptError()
      scene <- currentScene.addEntity(entityToAdd).adaptError()
    yield currentScene = scene

  override def removeEntity(entityId: String): Either[BaseError, Unit] =
    for
      simulatorEntity <- getEntity(entityId)
      engineEntity <- simulatorEntity.toEngineModel.adaptError()
      scene <- currentScene.removeEntity(engineEntity).adaptError()
    yield currentScene = scene

  override def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for
      entityId = command.entity.id
      _ <- removeEntity(entityId)
      _ <- createEntity(command)
    yield currentScene

  override def getAllSurfaces: List[MonadCoreSurface] =
    currentScene.surfaces.values.toList.map(_.toSimulationSurface)

  override def getSurface(id: String): Either[BaseError, MonadCoreSurface] =
    for
      id <- LocatableId(id).adaptError()
      surface <- currentScene.getSurface(id).toSimulationEitherSurface.adaptError()
    yield surface

  override def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      engineModel <- command.surface.toEngineModel.adaptError()
      scene <- currentScene.addSurface(engineModel).adaptError()
    yield currentScene = scene

  override def removeSurface(id: String): Either[BaseError, Unit] =
    for
      simulatorSurface <- getSurface(id)
      engineSurface <- simulatorSurface.toEngineModel.adaptError()
      scene <- currentScene.removeSurface(engineSurface).adaptError()
    yield currentScene = scene

  override def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      surfaceId = command.surface.id
      _ <- removeSurface(surfaceId)
      _ <- createSurface(command)
    yield currentScene

  override def getAllTeams: List[MonadCoreTeam] =
    currentScene.teams.values.toList.map(_.toSimulationTeam)

  override def getTeam(id: String): Either[BaseError, MonadCoreTeam] =
    for 
      id <- TeamId(id).adaptError()
      team <- currentScene.getTeam(id).toSimulationEitherTeam.adaptError()
    yield team
      
  override def createTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for
      engineTeam <- command.team.toEngineModel.adaptError()
      scene <- currentScene.addTeam(engineTeam).adaptError()
    yield currentScene = scene

  override def removeTeam(id: String): Either[BaseError, Unit] =
    for
      simulatorTeam <- getTeam(id)
      engineTeam <- simulatorTeam.toEngineModel.adaptError()
      scene <- currentScene.removeTeam(engineTeam).adaptError()
    yield currentScene = scene

  override def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for
      teamId = command.team.id
      _ <- removeTeam(teamId)
      _ <- createTeam(command)
    yield currentScene

  override def scene: Scene = currentScene
