package monad_core.simulator.application.engine.world

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{LocatableId, Surface}

case class SaveSurfaceCommand(
    surface: Surface
)

private[world] trait SurfaceOperations:
  def getAllSurfaces: List[Surface]

  def getSurface(id: LocatableId): Either[EngineError, Surface]

  def createSurface(command: SaveSurfaceCommand): Either[EngineError, Unit]

  def removeSurface(id: LocatableId): Either[EngineError, Unit]

  def updateSurface(command: SaveSurfaceCommand): Either[EngineError, Unit]
