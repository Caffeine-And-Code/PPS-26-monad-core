package monad_core.simulator.infrastructure.engine

import monad_core.engine.model.*
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.errors.BaseError

case class MonadCoreWorld(
    initialScene: Scene = Scene()
) extends World:

  var currentScene: Scene = initialScene

  override def getAllEntities: List[Entity] =
    currentScene.entities.values.toList

  override def getEntity(entityId: String): Either[BaseError, Entity] =
    for
      id     <- LocatableId(entityId).adaptError()
      entity <- currentScene.getEntity(id).adaptError()
    yield entity

  override def createEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for scene <- currentScene.addEntity(command.entity).adaptError()
    yield currentScene = scene

  override def removeEntity(entityId: String): Either[BaseError, Unit] =
    for
      entity <- getEntity(entityId)
      scene  <- currentScene.removeEntity(entity).adaptError()
    yield currentScene = scene

  override def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    for
      entityId = command.entity.id.value
      _ <- removeEntity(entityId)
      _ <- createEntity(command)
    yield currentScene

  override def getAllSurfaces: List[Surface] =
    currentScene.surfaces.values.toList

  override def getSurface(id: String): Either[BaseError, Surface] =
    for
      id      <- LocatableId(id).adaptError()
      surface <- currentScene.getSurface(id).adaptError()
    yield surface

  override def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      surface = command.surface
      scene <- currentScene.addSurface(surface).adaptError()
    yield currentScene = scene

  override def removeSurface(id: String): Either[BaseError, Unit] =
    for
      surface <- getSurface(id)
      scene   <- currentScene.removeSurface(surface).adaptError()
    yield currentScene = scene

  override def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    for
      surfaceId = command.surface.id.value
      _ <- removeSurface(surfaceId)
      _ <- createSurface(command)
    yield currentScene

  override def getAllTeams: List[Team] =
    currentScene.teams.values.toList

  override def getTeam(id: String): Either[BaseError, Team] =
    for
      id   <- TeamId(id).adaptError()
      team <- currentScene.getTeam(id).adaptError()
    yield team

  override def createTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for
      team = command.team
      scene <- currentScene.addTeam(team).adaptError()
    yield currentScene = scene

  override def removeTeam(id: String): Either[BaseError, Unit] =
    for
      team  <- getTeam(id)
      scene <- currentScene.removeTeam(team).adaptError()
    yield currentScene = scene

  override def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    for
      teamId = command.team.id.value
      _ <- removeTeam(teamId)
      _ <- createTeam(command)
    yield currentScene

  override def scene: Scene = currentScene
