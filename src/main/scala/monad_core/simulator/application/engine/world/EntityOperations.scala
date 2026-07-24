package monad_core.simulator.application.engine.world

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, LocatableId}

case class SaveEntityCommand(
                              entity: Entity
                            )

private[world] trait EntityOperations:
  def getAllEntities: List[Entity]

  def getEntity(entityId: LocatableId): Either[EngineError, Entity]

  def createEntity(command: SaveEntityCommand): Either[EngineError, World]

  def removeEntity(entityId: LocatableId): Either[EngineError, World]

  def updateEntity(command: SaveEntityCommand): Either[EngineError, World]

