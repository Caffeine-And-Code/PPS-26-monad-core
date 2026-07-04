package engine.core

import engine.core.events.Event

import scala.collection.immutable.Queue

case class EventManager(
                         queue: Queue[Event] = Queue.empty
                       )

extension (manager: EventManager)

  def registerEvent(event: Event): EventManager = manager.copy(queue = manager.queue.enqueue(event))
