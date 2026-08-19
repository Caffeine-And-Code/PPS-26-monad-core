package monad_core.engine.simulator

import monad_core.engine.core.events.Event
import monad_core.engine.core.events.Event.{
  EntityCollisionDetectedEvent,
  EntityCreatedEvent,
  EntityRemovedEvent,
  EntityUpdatedEvent
}
import monad_core.engine.core.events.handlers.{
  EntityCollisionDetectedEventHandler,
  EntityCreatedEventHandler,
  EntityRemovedEventHandler,
  EntityUpdatedEventHandler
}
import monad_core.engine.model.{EngineError, Scene}

object EventDispatcher:

  def handle(event: Event, scene: Scene): Either[EngineError, Scene] =
    event match
      case e: EntityCreatedEvent => EntityCreatedEventHandler.handle(e, scene)

      case e: EntityRemovedEvent => EntityRemovedEventHandler.handle(e, scene)

      case e: EntityUpdatedEvent => EntityUpdatedEventHandler.handle(e, scene)

      case e: EntityCollisionDetectedEvent => EntityCollisionDetectedEventHandler.handle(e, scene)
