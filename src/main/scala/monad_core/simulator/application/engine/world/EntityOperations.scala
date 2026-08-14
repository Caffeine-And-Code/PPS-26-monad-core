package monad_core.simulator.application.engine.world

import monad_core.simulator.domain.engine.MonadCoreEntity
import monad_core.simulator.errors.BaseError

case class SaveEntityCommand(
    entity: MonadCoreEntity
)

private[world] trait EntityOperations:
  def getAllEntities: Either[BaseError, List[MonadCoreEntity]]

  def getEntity(entityId: String): Either[BaseError, MonadCoreEntity]

  def createEntity(command: SaveEntityCommand): Either[BaseError, Unit]

  def removeEntity(entityId: String): Either[BaseError, Unit]

  def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit]
