package engine.core.events

import engine.core.Scene
import engine.model.LocatableId

sealed trait Event

object Event:

  case class EntityCreatedEvent(scene: Scene, createdEntityId: LocatableId) extends Event

  case class EntityRemovedEvent(scene: Scene, removedEntityId: LocatableId) extends Event

  case class EntityUpdatedEvent(scene: Scene, updatedEntityId: LocatableId) extends Event

  case class EntityCollisionDetectedEvent[M](scene: Scene, entityId: LocatableId, modelCollidedWith: M) extends Event