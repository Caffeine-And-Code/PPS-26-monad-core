package monad_core.engine.core.events.handlers

import monad_core.engine.core.events.Event.EntityRemovedEvent
import monad_core.engine.model.{EngineError, Scene}

object EntityRemovedEventHandler:

  def handle(event: EntityRemovedEvent, currentScene: Scene): Either[EngineError, Scene] =
    currentScene.removeEntity(event.entityToRemove)
