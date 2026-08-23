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
    initialMode: LoopMode
) extends World:

  private val lock         = new Object
  private var currentScene = initialScene
  private var currentMode  = initialMode

  override def resize(width: Double, height: Double): Either[BaseError, Unit] =
    lock.synchronized:
      currentScene.resize(width, height).adaptError().map { scene =>
        currentScene = scene
      }

  override def getAllEntities: List[Entity] =
    scene.entities.values.toList

  override def getEntity(entityId: String): Either[BaseError, Entity] =
    for
      id     <- LocatableId(entityId).adaptError()
      entity <- scene.getEntity(id).adaptError()
    yield entity

  override def createEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    applyEdit(CreateEntity(command.entity))

  override def removeEntity(entityId: String): Either[BaseError, Unit] =
    LocatableId(entityId).adaptError().flatMap(id => applyEdit(RemoveEntity(id)))

  override def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit] =
    applyEdit(UpdateEntity(command.entity))

  override def getAllSurfaces: List[Surface] =
    scene.surfaces.values.toList

  override def getSurface(id: String): Either[BaseError, Surface] =
    for
      id      <- LocatableId(id).adaptError()
      surface <- scene.getSurface(id).adaptError()
    yield surface

  override def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    applyEdit(CreateSurface(command.surface))

  override def removeSurface(id: String): Either[BaseError, Unit] =
    LocatableId(id).adaptError().flatMap(surfaceId => applyEdit(RemoveSurface(surfaceId)))

  override def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit] =
    applyEdit(UpdateSurface(command.surface))

  override def getAllTeams: List[Team] =
    scene.teams.values.toList

  override def getTeam(id: String): Either[BaseError, Team] =
    for
      id   <- TeamId(id).adaptError()
      team <- scene.getTeam(id).adaptError()
    yield team

  override def createTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    applyEdit(CreateTeam(command.team))

  override def removeTeam(id: String): Either[BaseError, Unit] =
    TeamId(id).adaptError().flatMap(teamId => applyEdit(RemoveTeam(teamId)))

  override def updateTeam(command: SaveTeamCommand): Either[BaseError, Unit] =
    applyEdit(UpdateTeam(command.team))

  override def scene: Scene =
    lock.synchronized(currentScene)

  override def replaceScene(scene: Scene): Unit =
    lock.synchronized:
      currentScene = scene

  override def enterEditMode(): Unit =
    lock.synchronized:
      currentMode = LoopMode.EditMode

  override def enterSimulationMode(): Unit =
    lock.synchronized:
      currentMode = LoopMode.SimulationMode

  private def applyEdit(edit: WorldEdit): Either[BaseError, Unit] =
    val editResult = lock.synchronized:
      WorldEditor(currentMode, currentScene, edit).map { result =>
        currentScene = result.scene
        result.events
      }

    editResult.foreach { events =>
      if events.nonEmpty then onEvents(events)
    }
    editResult.map(_ => ())

object MonadCoreWorld:

  def apply(
      initialScene: Scene = Scene(),
      onEvents: Vector[EngineEvent] => Unit = _ => (),
      initialMode: LoopMode = LoopMode.EditMode
  ): MonadCoreWorld =
    new MonadCoreWorld(initialScene, onEvents, initialMode)
