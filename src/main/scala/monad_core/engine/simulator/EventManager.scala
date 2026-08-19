package monad_core.engine.simulator

import monad_core.engine.core.events.Event
import monad_core.engine.model.{EngineError, Scene}

import scala.collection.immutable.Queue

case class EventManager(
    queue: Queue[Event] = Queue.empty
)

extension (manager: EventManager)

  def registerEvent(event: Event): EventManager = manager.copy(queue = manager.queue.enqueue(event))

  def registerEvents(events: Iterable[Event]): EventManager =
    manager.copy(queue = manager.queue.enqueueAll(events))

  def dispatchEvents(initialScene: Scene)(
      handle: (Event, Scene) => Either[EngineError, Scene]
  ): (Either[EngineError, Scene], EventManager) =
    val result = manager.queue.foldLeft(Right(initialScene): Either[EngineError, Scene]) {
      (acc, event) =>
        acc.flatMap(scene => handle(event, scene))
    }
    (result, manager.copy(queue = Queue.empty))
