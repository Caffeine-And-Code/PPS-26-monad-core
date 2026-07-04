package engine.core

import engine.core.events.Event
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.Queue

class EventManagerTest extends AnyFunSuite with Matchers with MockFactory:

  val mockedEvent: Event = mock[Event]

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

