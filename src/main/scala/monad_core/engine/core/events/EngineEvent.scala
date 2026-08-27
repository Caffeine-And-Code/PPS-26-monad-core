package monad_core.engine.core.events

import monad_core.engine.geometry.Collision
import monad_core.engine.model.{BorderSide, Entity, LocatableId}

/**
 * Event emitted by the engine.
 *
 * Events describe completed state transitions and can be consumed by external observers
 * without exposing the engine implementation that produced them.
 */
sealed trait EngineEvent

/** Constructors for the events. */
object EngineEvent:

  /**
   * Signals that an entity has been added to the engine state.
   *
   * @param entity entity present in the state after the operation
   */
  final case class EntityCreated(entity: Entity) extends EngineEvent

  /**
   * Signals that an entity has been removed from the engine state.
   *
   * @param entity entity as it appeared immediately before removal
   */
  final case class EntityRemoved(entity: Entity) extends EngineEvent

  /**
   * Describes a change to an existing entity.
   *
   * @param previous entity state before the update
   * @param current entity state after the update
   */
  final case class EntityUpdated(previous: Entity, current: Entity) extends EngineEvent

  /**
   * Describes a collision involving an entity and another collision target.
   *
   * @param entityId identifier of the primary entity involved in the collision
   * @param target entity, surface, or world border hit by the primary entity
   * @param collision geometric collision data produced by the collision detector
   */
  final case class CollisionDetected(
      entityId: LocatableId,
      target: CollisionTarget,
      collision: Collision
  ) extends EngineEvent

/** Identifies the target hit by an entity during a collision. */
enum CollisionTarget:
  /** Another entity identified by its locatable identifier. */
  case Entity(id: LocatableId)

  /** A surface identified by its locatable identifier. */
  case Surface(id: LocatableId)

  /** One of the world boundaries. */
  case Border(side: BorderSide)
