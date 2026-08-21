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

final case class EventLogEntry(level: EventLogLevel, message: String)

def formatEvent(event: EngineEvent): EventLogEntry =
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

def formatEvents(events: Iterable[EngineEvent]): Vector[EventLogEntry] =
  events.iterator.map(formatEvent).toVector

private def formatCollisionTarget(target: CollisionTarget): (String, String) =
  target match
    case CollisionTarget.Entity(id)  => "entity"  -> id.value
    case CollisionTarget.Surface(id) => "surface" -> id.value
    case CollisionTarget.Border(side) =>
      "border" -> formatBorderSide(side)

private def formatBorderSide(side: BorderSide): String =
  side match
    case BorderSide.Left   => "left"
    case BorderSide.Right  => "right"
    case BorderSide.Top    => "top"
    case BorderSide.Bottom => "bottom"
