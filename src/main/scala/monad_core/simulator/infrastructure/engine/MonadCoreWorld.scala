package monad_core.simulator.infrastructure.engine

import monad_core.engine.core.LoopMode
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.model.*
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.world.WorldEdit.*
import monad_core.simulator.infrastructure.engine.world.{WorldEdit, WorldEditor}

final class MonadCoreWorld(
    initialScene: Scene,
    onEvents: Vector[EngineEvent] => Unit,
    currentMode: () => LoopMode
) extends World:

  var currentScene: Scene = initialScene

  override def resize(width: Double, height: Double): Either[BaseError, Unit] =
    currentScene.resize(width, height).adaptError().map { scene =>
      currentScene = scene
    }

  override def getAllEntities: List[Entity] =
    currentScene.entities.values.toList

  override def getEntity(entityId: String): Either[BaseError, Entity] =
    for
      id     <- LocatableId(entityId).adaptError()
      entity <- currentScene.getEntity(id).adaptError()
    yield entity

  override def createEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    applyEdit(CreateEntity(command.entity))

  override def removeEntity(entityId: String): Either[BaseError, Unit] =
    LocatableId(entityId).adaptError().flatMap(id => applyEdit(RemoveEntity(id)))

  override def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    applyEdit(UpdateEntity(command.entity))

  override def getAllSurfaces: List[Surface] =
    currentScene.surfaces.values.toList

  override def getSurface(id: String): Either[BaseError, Surface] =
    for
      id      <- LocatableId(id).adaptError()
      surface <- currentScene.getSurface(id).adaptError()
    yield surface

  override def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    applyEdit(CreateSurface(command.surface))

  override def removeSurface(id: String): Either[BaseError, Unit] =
    LocatableId(id).adaptError().flatMap(surfaceId => applyEdit(RemoveSurface(surfaceId)))

  override def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    applyEdit(UpdateSurface(command.surface))

  override def getAllTeams: List[Team] =
    currentScene.teams.values.toList

  override def getTeam(id: String): Either[BaseError, Team] =
    for
      id   <- TeamId(id).adaptError()
      team <- currentScene.getTeam(id).adaptError()
    yield team

  override def createTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    applyEdit(CreateTeam(command.team))

  override def removeTeam(id: String): Either[BaseError, Unit] =
    TeamId(id).adaptError().flatMap(teamId => applyEdit(RemoveTeam(teamId)))

  override def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    applyEdit(UpdateTeam(command.team))

  override def scene: Scene = currentScene

  private def applyEdit(edit: WorldEdit): Either[BaseError, Unit] =
    WorldEditor(currentMode(), currentScene, edit).map { result =>
      currentScene = result.scene
      if result.events.nonEmpty then onEvents(result.events)
    }

object MonadCoreWorld:

  def apply(
      initialScene: Scene = Scene(),
      onEvents: Vector[EngineEvent] => Unit = _ => (),
      currentMode: () => LoopMode = () => LoopMode.EditMode
  ): MonadCoreWorld =
    new MonadCoreWorld(initialScene, onEvents, currentMode)
