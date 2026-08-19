package monad_core.engine.core

import monad_core.engine.core.events.Event
import monad_core.engine.model.EngineError

import scala.collection.immutable.Queue

case class EventManager(
    queue: Queue[Event] = Queue.empty
)

extension (manager: EventManager)

  def registerEvent(event: Event): EventManager = manager.copy(queue = manager.queue.enqueue(event))

  def dispatchEvents[S](initialScene: S)(
      handle: (Event, S) => Either[EngineError, S]
  ): (Either[EngineError, S], EventManager) =
    val result = manager.queue.foldLeft(Right(initialScene): Either[EngineError, S]) {
      (acc, event) =>
        acc.flatMap(scene => handle(event, scene))
    }
    (result, manager.copy(queue = Queue.empty))
