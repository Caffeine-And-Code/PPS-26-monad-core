package monad_core.engine.core.events.handlers

import monad_core.engine.core.Scene
import monad_core.engine.core.events.Event.EntityRemovedEvent
import monad_core.engine.errors.EngineError

object EntityRemovedEventHandler:

  def handle(event: EntityRemovedEvent, currentScene: Scene): Either[EngineError, Scene] =
    currentScene.removeEntity(event.entityToRemove)
