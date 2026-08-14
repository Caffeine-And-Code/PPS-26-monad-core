package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.Scene
import monad_core.engine.model.*
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.domain.engine.{
  MonadCoreEntity,
  MonadCoreScene,
  MonadCoreSurface,
  MonadCoreTeam
}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.infrastructure.engine.translators.BaseTranslator.{
  toSimulationEitherEntity,
  toSimulationEitherSurface,
  toSimulationEitherTeam
}
import monad_core.simulator.infrastructure.engine.translators.EntityTranslator.{
  toEngineModel,
  toSimulationEntity
}
import monad_core.simulator.infrastructure.engine.translators.SurfaceTranslator.{
  toEngineModel,
  toSimulationSurface
}
import monad_core.simulator.infrastructure.engine.translators.TeamTranslator.{
  toEngineModel,
  toSimulationTeam
}
import monad_core.simulator.infrastructure.engine.translators.SceneTranslator.{
  toEngineModel,
  toSimulationScene
}

case class MonadCoreWorld(
    initialScene: MonadCoreScene = MonadCoreScene()
) extends World:

  var currentScene: MonadCoreScene = initialScene

  private def getEngineScene: Either[BaseError, Scene] =
    currentScene.toEngineModel.adaptError()

  override def getAllEntities: Either[BaseError, List[MonadCoreEntity]] =
    for scene <- getEngineScene
    yield scene.entities.values.toList.map(_.toSimulationEntity)

  override def getEntity(entityId: String): Either[BaseError, MonadCoreEntity] =
    for
      id     <- LocatableId(entityId).adaptError()
      scene  <- getEngineScene
      entity <- scene.getEntity(id).toSimulationEitherEntity.adaptError()
    yield entity

  override def createEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for
      entityToAdd    <- command.entity.toEngineModel.adaptError()
      convertedScene <- getEngineScene
      scene          <- convertedScene.addEntity(entityToAdd).adaptError()
    yield currentScene = scene.toSimulationScene

  override def removeEntity(entityId: String): Either[BaseError, Unit] =
    for
      simulatorEntity <- getEntity(entityId)
      engineEntity    <- simulatorEntity.toEngineModel.adaptError()
      convertedScene  <- getEngineScene
      scene           <- convertedScene.removeEntity(engineEntity).adaptError()
    yield currentScene = scene.toSimulationScene

  override def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for
      entityId = command.entity.id
      _ <- removeEntity(entityId)
      _ <- createEntity(command)
    yield currentScene

  override def getAllSurfaces: Either[BaseError, List[MonadCoreSurface]] =
    for scene <- getEngineScene
    yield scene.surfaces.values.toList.map(_.toSimulationSurface)

  override def getSurface(id: String): Either[BaseError, MonadCoreSurface] =
    for
      id             <- LocatableId(id).adaptError()
      convertedScene <- getEngineScene
      surface        <- convertedScene.getSurface(id).toSimulationEitherSurface.adaptError()
    yield surface

  override def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      engineModel    <- command.surface.toEngineModel.adaptError()
      convertedScene <- getEngineScene
      scene          <- convertedScene.addSurface(engineModel).adaptError()
    yield currentScene = scene.toSimulationScene

  override def removeSurface(id: String): Either[BaseError, Unit] =
    for
      simulatorSurface <- getSurface(id)
      engineSurface    <- simulatorSurface.toEngineModel.adaptError()
      convertedScene   <- getEngineScene
      scene            <- convertedScene.removeSurface(engineSurface).adaptError()
    yield currentScene = scene.toSimulationScene

  override def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      surfaceId = command.surface.id
      _ <- removeSurface(surfaceId)
      _ <- createSurface(command)
    yield currentScene

  override def getAllTeams: Either[BaseError, List[MonadCoreTeam]] =
    for scene <- getEngineScene
    yield scene.teams.values.toList.map(_.toSimulationTeam)

  override def getTeam(id: String): Either[BaseError, MonadCoreTeam] =
    for
      id             <- TeamId(id).adaptError()
      convertedScene <- getEngineScene
      team           <- convertedScene.getTeam(id).toSimulationEitherTeam.adaptError()
    yield team

  override def createTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for
      engineTeam     <- command.team.toEngineModel.adaptError()
      convertedScene <- getEngineScene
      scene          <- convertedScene.addTeam(engineTeam).adaptError()
    yield currentScene = scene.toSimulationScene

  override def removeTeam(id: String): Either[BaseError, Unit] =
    for
      simulatorTeam  <- getTeam(id)
      engineTeam     <- simulatorTeam.toEngineModel.adaptError()
      convertedScene <- getEngineScene
      scene          <- convertedScene.removeTeam(engineTeam).adaptError()
    yield currentScene = scene.toSimulationScene

  override def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for
      teamId = command.team.id
      _ <- removeTeam(teamId)
      _ <- createTeam(command)
    yield currentScene

  override def scene: MonadCoreScene = currentScene
