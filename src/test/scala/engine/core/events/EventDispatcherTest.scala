package engine.core.events

import engine.core.events.Event.{EntityCreatedEvent, EntityRemovedEvent}
import engine.core.*
import engine.errors.EngineError
import engine.model.{Entity, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EventDispatcherTest extends AnyFunSuite with Matchers:

  val scene: Scene = Scene()
  val entity: Entity = Entity.circle("genericEntity", Vector2D(0, 0), 2).value

  test("EntityCreatedEvent is dispatched correctly"):
    val creationEvent = EntityCreatedEvent(entity)

    val result: Either[EngineError, (Scene, Entity)] =
      for
        updatedScene <- EventDispatcher.handle(creationEvent, scene)
        fetched <- updatedScene.getEntity(entity.id)
      yield (updatedScene, fetched)

    inside(result):
      case Right((updatedScene, fetched)) =>
        updatedScene.entities.size should be(1)
        fetched should be(entity)

  test("EntityCreatedEvent errors are curried throughout the handle function"):
    val creationEvent = EntityCreatedEvent(entity)
    val populatedScene: Scene = scene.addEntity(entity).value
    val expectedError = CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id))

    val result = EventDispatcher.handle(creationEvent, populatedScene)

    inside(result):
      case Left(error) =>
        error should be(expectedError)

  test("EntityRemovedEvent is dispatched correctly"):
    val removeEvent = EntityRemovedEvent(entity)
    val populatedScene: Scene = scene.addEntity(entity).value

    val result = EventDispatcher.handle(removeEvent, populatedScene)

    inside(result):
      case Right(updatedScene) =>
        updatedScene.entities.size should be(0)

  test("EntityRemovedEvent errors are curried throughout the handle function"):
    val removeEvent = EntityRemovedEvent(entity)
    val expectedError = CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id))

    val result = EventDispatcher.handle(removeEvent, scene)

    inside(result):
      case Left(error) =>
        error should be(expectedError)