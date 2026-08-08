package monad_core.engine.core.events.handlers

import monad_core.engine.core.Scene
import monad_core.engine.core.events.Event.EntityCollisionDetectedEvent
import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Surface}

object EntityCollisionDetectedEventHandler:

  def handle(event: EntityCollisionDetectedEvent, currentScene: Scene): Either[EngineError, Scene] =
    event.modelCollidedWith match
      case _: Entity => handleCollisionToAnotherEntity(event, currentScene)
      case _: Surface => handleCollisionToSurface(event, currentScene)

  private def handleCollisionToAnotherEntity(
                                              collisionToEntityEvent: EntityCollisionDetectedEvent,
                                              currentScene: Scene
                                            ): Either[EngineError, Scene] =
    //handle the collision to another entity
    Right(currentScene)

  private def handleCollisionToSurface(
                                        collisionToSurfaceEvent: EntityCollisionDetectedEvent,
                                        currentScene: Scene
                                      ): Either[EngineError, Scene] =
    //handle the collision to a surface
    Right(currentScene)