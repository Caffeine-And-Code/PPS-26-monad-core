package monad_core.engine.core.events

import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, LocatableId, Surface}

type CollidableModels = Entity | Surface

sealed trait Event

object Event:

  case class EntityCreatedEvent(entityToAdd: Entity) extends Event

  case class EntityRemovedEvent(entityToRemove: Entity) extends Event

  case class EntityUpdatedEvent(entityToUpdate: Entity) extends Event

  case class EntityCollisionDetectedEvent(
      entityId: LocatableId,
      modelCollidedWith: CollidableModels,
      collisionData: Collision
  ) extends Event
