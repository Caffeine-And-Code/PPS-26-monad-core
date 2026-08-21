package monad_core.engine.core.events

import monad_core.engine.geometry.Collision
import monad_core.engine.model.{BorderSide, Entity, LocatableId}

sealed trait EngineEvent

object EngineEvent:

  final case class EntityCreated(entity: Entity) extends EngineEvent

  final case class EntityRemoved(entity: Entity) extends EngineEvent

  final case class EntityUpdated(previous: Entity, current: Entity) extends EngineEvent

  final case class CollisionDetected(
      entityId: LocatableId,
      target: CollisionTarget,
      collision: Collision
  ) extends EngineEvent

enum CollisionTarget:
  case Entity(id: LocatableId)
  case Surface(id: LocatableId)
  case Border(side: BorderSide)
