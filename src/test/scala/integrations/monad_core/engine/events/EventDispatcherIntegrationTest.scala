package integrations.monad_core.engine.events

import monad_core.engine.core.events.Event.{EntityCreatedEvent, EntityRemovedEvent, EntityUpdatedEvent}
import monad_core.engine.core.events.EventDispatcher
import monad_core.engine.core.*
import monad_core.engine.model.{EngineError, Entity, Scene, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EventDispatcherIntegrationTest extends AnyFunSuite with Matchers:

  val scene: Scene   = Scene()
  val entity: Entity = Entity.circle("genericEntity", Vector2D(0, 0), 2).value

  test("EntityCreatedEvent is dispatched correctly"):
    val creationEvent = EntityCreatedEvent(entity)

    val result: Either[EngineError, (Scene, Entity)] =
      for
        updatedScene <- EventDispatcher.handle(creationEvent, scene)
        fetched      <- updatedScene.getEntity(entity.id)
      yield (updatedScene, fetched)

    inside(result):
      case Right((updatedScene, fetched)) =>
        updatedScene.entities.size should be(1)
        fetched should be(entity)

  test("EntityCreatedEvent errors are curried throughout the handle function"):
    val creationEvent = EntityCreatedEvent(entity)
    val expectedError = CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id))

    val dispatchResult = for {
      populatedScene <- scene.addEntity(entity)
      result         <- EventDispatcher.handle(creationEvent, populatedScene)
    } yield result

    inside(dispatchResult):
      case Left(error) =>
        error should be(expectedError)

  test("EntityRemovedEvent is dispatched correctly"):
    val removeEvent = EntityRemovedEvent(entity)

    val dispatchResult = for {
      populatedScene <- scene.addEntity(entity)
      result         <- EventDispatcher.handle(removeEvent, populatedScene)
    } yield result

    inside(dispatchResult):
      case Right(updatedScene) =>
        updatedScene.entities.size should be(0)

  test("EntityRemovedEvent errors are curried throughout the handle function"):
    val removeEvent   = EntityRemovedEvent(entity)
    val expectedError = CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id))

    val result = EventDispatcher.handle(removeEvent, scene)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("EntityUpdatedEvent is dispatched correctly"):
    val updatedEvent = EntityUpdatedEvent(entity)

    val dispatchResult = for
      populatedScene <- scene.addEntity(entity)
      result         <- EventDispatcher.handle(updatedEvent, populatedScene)
    yield result

    inside(dispatchResult):
      case Right(updatedScene) =>
        updatedScene.entities.size should be(1)
        updatedScene.entities should contain value entity

  test("EntityUpdatedEvent errors are curried throughout the handle function"):
    val updatedEvent  = EntityUpdatedEvent(entity)
    val expectedError = CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id))

    val result = EventDispatcher.handle(updatedEvent, scene)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

//TODO: add test and integration for the EntityCollisionDetectedEvent when the team decided what needs to be done
