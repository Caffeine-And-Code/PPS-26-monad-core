package engine.core.events.handlers

import engine.core.Scene
import engine.core.events.Event.EntityCreatedEvent
import engine.errors.EngineError

object EntityCreatedEventHandler:

  def handle(event: EntityCreatedEvent, currentScene: Scene): Either[EngineError, Scene] =
    currentScene.addEntity(event.entityToAdd)