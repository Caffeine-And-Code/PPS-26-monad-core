package engine.core.events.handlers

import engine.core.Scene
import engine.core.events.Event.EntityUpdatedEvent
import engine.errors.EngineError

object EntityUpdatedEventHandler:

  def handle(event: EntityUpdatedEvent, currentScene: Scene): Either[EngineError, Scene] =
    //handle the event
    Right(currentScene)
