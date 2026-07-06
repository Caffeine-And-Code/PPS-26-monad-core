package engine.core.events.handlers

import engine.core.Scene
import engine.core.events.Event.EntityRemovedEvent
import engine.errors.EngineError

object EntityRemovedEventHandler:

  def handle(event: EntityRemovedEvent, currentScene: Scene): Either[EngineError, Scene] =
    currentScene.removeEntity(event.entityToRemove)
