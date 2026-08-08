package monad_core.engine.core.events.handlers

import monad_core.engine.core.events.Event.EntityCreatedEvent
import monad_core.engine.core.Scene
import monad_core.engine.errors.EngineError

object EntityCreatedEventHandler:

  def handle(event: EntityCreatedEvent, currentScene: Scene): Either[EngineError, Scene] =
    currentScene.addEntity(event.entityToAdd)