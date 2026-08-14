package monad_core.simulator.application.engine.world

import monad_core.simulator.domain.engine.MonadCoreSurface
import monad_core.simulator.errors.BaseError

case class SaveSurfaceCommand(
    surface: MonadCoreSurface
)

private[world] trait SurfaceOperations:
  def getAllSurfaces: Either[BaseError, List[MonadCoreSurface]]

  def getSurface(id: String): Either[BaseError, MonadCoreSurface]

  def createSurface(command: SaveSurfaceCommand): Either[BaseError, Unit]

  def removeSurface(id: String): Either[BaseError, Unit]

  def updateSurface(command: SaveSurfaceCommand): Either[BaseError, Unit]
