package engine.core

import engine.core.events.Event
import engine.errors.EngineError
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.Queue

class EventManagerTest extends AnyFunSuite with Matchers with MockFactory:
  
  val mockedEvent: Event = mock[Event]
  val mockedScene: Scene = Scene()
  val mockedError: EngineError = mock[EngineError]

  def errorReturningHandleFunction(event: Event, scene: Scene): Either[EngineError, Scene] =
    Left(mockedError)

  def correctHandleFunction(event: Event, scene: Scene): Either[EngineError, Scene] =
    Right(scene)

  val defaultManager = EventManager()

  test("event queue upon initialization is empty"):
    val manager = EventManager()

    manager.queue should be(Queue.empty)

  test("event queue can be initialized with a populated queue"):
    val populatedQueue = Queue(mockedEvent)

    val manager = EventManager(populatedQueue)

    manager.queue should be(populatedQueue)

  test("an event is registered correctly"):
    val expectedQueue = Queue(mockedEvent)

    val populatedQueueManager = defaultManager.registerEvent(mockedEvent)

    populatedQueueManager.queue should be(expectedQueue)
    
  test("processing an empty queue doesn't change the given state"):
    val (dispatchResult, updatedManager) = defaultManager.dispatchEvents(mockedScene)(correctHandleFunction)

    inside(dispatchResult):
      case Right(processedScene) =>
        updatedManager.queue.length should be(0)
        processedScene should be(mockedScene)


  test("an enqueued event is processed correctly"):
    val populatedManager = defaultManager.registerEvent(mockedEvent)

    val (dispatchResult, updatedManager) = populatedManager.dispatchEvents(mockedScene)(correctHandleFunction)

    inside(dispatchResult):
      case Right(_) =>
        updatedManager.queue.length should be(0)

  test("when a error occurs during dispatch, it is curried and the manager is updated correctly"):
    val populatedManager = defaultManager.registerEvent(mockedEvent)

    val (dispatchResult, updatedManager) = populatedManager.dispatchEvents(mockedScene)(errorReturningHandleFunction)

    inside(dispatchResult):
      case Left(curriedError) =>
        curriedError should be(mockedError)
        updatedManager.queue.length should be(0)