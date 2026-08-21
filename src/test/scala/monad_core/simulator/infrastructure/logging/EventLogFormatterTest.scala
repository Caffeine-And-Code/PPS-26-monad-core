package monad_core.simulator.infrastructure.logging

import monad_core.engine.core.events.EngineEvent.{
  CollisionDetected,
  EntityCreated,
  EntityRemoved,
  EntityUpdated
}
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{BorderSide, Entity, Surface, Vector2D}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EventLogFormatterTest extends AnyFunSuite with Matchers:

  private val entity  = Entity.circle("entity-1", Vector2D(10, 20), 2).value
  private val surface = Surface.circle("surface-1", Vector2D(30, 40), 3).value

  test("formats lifecycle events at info level"):
    formatEvents(Vector(EntityCreated(entity), EntityRemoved(entity))) shouldBe Vector(
      EventLogEntry(EventLogLevel.Info, "event=entity_created entity_id=entity-1"),
      EventLogEntry(EventLogLevel.Info, "event=entity_removed entity_id=entity-1")
    )

  test("formats entity updates at trace level"):
    val previous = entity.moveTo(Vector2D(0, 0))

    formatEvent(EntityUpdated(previous, entity)) shouldBe EventLogEntry(
      EventLogLevel.Trace,
      "event=entity_updated entity_id=entity-1 " +
        "previous_position_x=0.0 previous_position_y=0.0 " +
        "current_position_x=10.0 current_position_y=20.0"
    )

  test("formats an entity collision target"):
    val collision = Collision(Vector2D(0, 1), 2.5)

    formatEvent(
      CollisionDetected(entity.id, CollisionTarget.Entity(entity.id), collision)
    ) shouldBe EventLogEntry(
      EventLogLevel.Info,
      "event=entity_collision_detected entity_id=entity-1 target_type=entity target_id=entity-1 " +
        "normal_x=0.0 normal_y=1.0 penetration_depth=2.5"
    )

  test("formats a surface collision target"):
    val collision = Collision(Vector2D(1, 0), 1.5)

    formatEvent(
      CollisionDetected(entity.id, CollisionTarget.Surface(surface.id), collision)
    ) shouldBe EventLogEntry(
      EventLogLevel.Info,
      "event=entity_collision_detected entity_id=entity-1 target_type=surface target_id=surface-1 " +
        "normal_x=1.0 normal_y=0.0 penetration_depth=1.5"
    )

  test("formats a border collision target"):
    val collision = Collision(Vector2D(1, 0), 1.5)

    formatEvent(
      CollisionDetected(entity.id, CollisionTarget.Border(BorderSide.Left), collision)
    ) shouldBe EventLogEntry(
      EventLogLevel.Info,
      "event=entity_collision_detected entity_id=entity-1 target_type=border target_id=left " +
        "normal_x=1.0 normal_y=0.0 penetration_depth=1.5"
    )
