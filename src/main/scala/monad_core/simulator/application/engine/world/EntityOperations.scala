package monad_core.simulator.application.engine.world

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, LocatableId}
import monad_core.simulator.errors.BaseError

case class SaveEntityCommand(
                              entity: Entity
                            )

private[world] trait EntityOperations:
  def getAllEntities: List[Entity]

  def getEntity(entityId: LocatableId): Either[BaseError, Entity]

  def createEntity(command: SaveEntityCommand): Either[BaseError, Unit]

  def removeEntity(entityId: LocatableId): Either[BaseError, Unit]

  def updateEntity(command: SaveEntityCommand): Either[BaseError, Unit]

