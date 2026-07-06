package engine.core.events

import engine.core.Scene
import engine.core.events.Event.{EntityCollisionDetectedEvent, EntityCreatedEvent, EntityRemovedEvent, EntityUpdatedEvent}
import engine.core.events.handlers.{EntityCollisionDetectedEventHandler, EntityCreatedEventHandler, EntityRemovedEventHandler, EntityUpdatedEventHandler}
import engine.errors.EngineError

object EventDispatcher:
  def handle(event: Event, scene: Scene): Either[EngineError, Scene] =
    event match
      case e: EntityCreatedEvent => EntityCreatedEventHandler.handle(e, scene)

      case e: EntityRemovedEvent => EntityRemovedEventHandler.handle(e, scene)

      case e: EntityUpdatedEvent => EntityUpdatedEventHandler.handle(e, scene)

      case e: EntityCollisionDetectedEvent => EntityCollisionDetectedEventHandler.handle(e, scene)
