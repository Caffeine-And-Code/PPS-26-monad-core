package monad_core.simulator.application.engine.world

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{LocatableId, Surface}
import monad_core.simulator.errors.BaseError

case class SaveSurfaceCommand(
                               surface: Surface
                             )

private[world] trait SurfaceOperations:
  def getAllSurfaces: List[Surface]

  def getSurface(id: LocatableId): Either[BaseError, Surface]

  def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit]

  def removeSurface(id: LocatableId): Either[BaseError, Unit]

  def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit]
