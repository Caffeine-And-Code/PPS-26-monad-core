package monad_core.simulator.infrastructure.logging

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.events.EngineEvent.{
  CollisionDetected,
  EntityCreated,
  EntityRemoved,
  EntityUpdated
}
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.model.BorderSide
import monad_core.engine.model.LocatableId.value

enum EventLogLevel:
  case Info, Trace

/**
 * Record class representing a single message that needs to be printed
 * @param level enum value representing the severity of the log itself
 * @param message the message to be printed
 */
final case class EventLogEntry(level: EventLogLevel, message: String)

/**
 * Map the provided event to the corresponding [[EventLogEntry]]
 *
 * @param event the event to map
 * @return formatted `EventLogEntry` record ready to be printed
 */
def mapEventToLogEntry(event: EngineEvent): EventLogEntry =
  event match
    case EntityCreated(entity) =>
      EventLogEntry(
        EventLogLevel.Info,
        s"event=entity_created entity_id=${entity.id.value}"
      )

    case EntityRemoved(entity) =>
      EventLogEntry(
        EventLogLevel.Info,
        s"event=entity_removed entity_id=${entity.id.value}"
      )

    case EntityUpdated(previous, current) =>
      EventLogEntry(
        EventLogLevel.Trace,
        s"event=entity_updated entity_id=${current.id.value} " +
          s"previous_position_x=${previous.position.x} previous_position_y=${previous.position.y} " +
          s"current_position_x=${current.position.x} current_position_y=${current.position.y}"
      )

    case CollisionDetected(entityId, target, collision) =>
      val (targetType, targetId) = formatCollisionTarget(target)

      EventLogEntry(
        EventLogLevel.Info,
        s"event=entity_collision_detected entity_id=${entityId.value} target_type=$targetType target_id=$targetId " +
          s"normal_x=${collision.normalVector.x} normal_y=${collision.normalVector.y} penetration_depth=${collision.penetrationDepth}"
      )

/**
 * Map all the provided events to the corresponding [[EventLogEntry]]
 *
 * @see [[mapEventToLogEntry]]
 * @param events the events to map
 * @return `Vector[EventLogEntry]`
 */
def mapEventsToLogEntries(events: Iterable[EngineEvent]): Vector[EventLogEntry] =
  events.iterator.map(mapEventToLogEntry).toVector

/**
 * Map each possible collision target to it's corresponding display value
 *
 * @see [[CollisionTarget]] and [[formatBorderSide]]
 * @param target a target of a collision
 * @return `(String, String)` The first string is the display value of the target like "entity", "surface" and "border".
 *
 *         The second is it's id for the entities and the surfaces, or it's side if it is a wall collision
 */
private def formatCollisionTarget(target: CollisionTarget): (String, String) =
  target match
    case CollisionTarget.Entity(id)  => "entity"  -> id.value
    case CollisionTarget.Surface(id) => "surface" -> id.value
    case CollisionTarget.Border(side) =>
      "border" -> formatBorderSide(side)

/**
 * Given a [[BorderSide]] it's display value is returned
 *
 * @param side enum value
 * @return the display value for the log
 */
private def formatBorderSide(side: BorderSide): String =
  side match
    case BorderSide.Left   => "left"
    case BorderSide.Right  => "right"
    case BorderSide.Top    => "top"
    case BorderSide.Bottom => "bottom"
