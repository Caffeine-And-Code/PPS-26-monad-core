package monad_core.engine.core.events.handlers

import monad_core.engine.core.events.Event.EntityUpdatedEvent
import monad_core.engine.model.{EngineError, Scene}

object EntityUpdatedEventHandler:

  def handle(event: EntityUpdatedEvent, currentScene: Scene): Either[EngineError, Scene] =
    for
      sceneWithoutEntity <- currentScene.removeEntity(event.entityToUpdate)
      finalScene         <- sceneWithoutEntity.addEntity(event.entityToUpdate)
    yield finalScene
