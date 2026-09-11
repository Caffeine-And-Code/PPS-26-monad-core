package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, LocatableId, Surface}

/**
 * Collision contact between two entities.
 *
 * @see [[monad_core.engine.geometry.Collision Collision]] and [[PhysicsContext.detect]]
 * @param firstId identifier of the first entity
 * @param secondId identifier of the second entity
 * @param collision geometric data produced by the
 *                  [[monad_core.engine.collision_detection.CollisionDetector CollisionDetector]]
 */
final case class EntityCollisionContact(
    firstId: LocatableId,
    secondId: LocatableId,
    collision: Collision
)

/**
 * Contact between an entity and a surface that contains it.
 *
 * @see [[PhysicsContext.detect]]
 * @param entityId identifier of the entity
 * @param surfaceId identifier of the surface
 */
final case class SurfaceContact(
    entityId: LocatableId,
    surfaceId: LocatableId
)

/**
 * Immutable collection of contacts detected for one physics step.
 *
 * The snapshot allows every physics rule to consume the same collision data without
 * running collision detection again.
 *
 * @see [[PhysicsContext.detect]]
 * @param entityContacts contacts between pairs of entities
 * @param surfaceContacts contacts between entities and surfaces
 */
final case class CollisionSnapshot(
    entityContacts: Vector[EntityCollisionContact] = Vector.empty,
    surfaceContacts: Vector[SurfaceContact] = Vector.empty
)

/**
 * Input shared by all physics rules during one simulation step.
 *
 * @see [[CollisionSnapshot]], [[monad_core.engine.core.traits.State State]] and
 *      [[monad_core.engine.physics.core.PhysicsRule PhysicsRule]]
 * @param state state at the beginning of the physics step
 * @param dt elapsed simulation time represented in nanoseconds
 * @param collisions contacts detected for the current step
 */
final case class PhysicsContext(
    state: State,
    dt: Long,
    collisions: CollisionSnapshot = CollisionSnapshot()
)

/** Factory and lookup utilities for [[PhysicsContext]]. */
object PhysicsContext:
  private val CombinationSize = 2

  /**
   * Indexes the context entities by identifier.
   *
   * @param context physics context to inspect
   * @return map from entity identifiers to entities
   */
  def getEntityMapById(context: PhysicsContext): Map[LocatableId, Entity] =
    context.state.allEntities.map(entity => entity.id -> entity).toMap

  /**
   * Indexes the context surfaces by identifier.
   *
   * @param context physics context to inspect
   * @return map from surface identifiers to surfaces
   */
  def getSurfaceMapById(context: PhysicsContext): Map[LocatableId, Surface] =
    context.state.allSurfaces.map(surface => surface.id -> surface).toMap

  /**
   * Detects contacts and creates the initial context for a physics-rule sequence.
   *
   * Entity pairs where both entities are fixed are ignored. Surface contacts are detected
   * only for non-fixed entities.
   *
   * @param state state at the beginning of the physics step
   * @param dt elapsed simulation time represented in nanoseconds
   * @param detector collision detector used for entity and surface checks
   * @return a context containing the input state, elapsed time, and detected contacts
   */
  def detect(state: State, dt: Long)(using detector: CollisionDetector): PhysicsContext =
    val entities = state.allEntities
    val surfaces = state.allSurfaces

    val entityContacts = entities
      .combinations(CombinationSize)
      .collect:
        case Seq(first, second) if !(first.isFixed && second.isFixed) =>
          detector
            .collision(first, second)
            .map(EntityCollisionContact(first.id, second.id, _))
      .flatten
      .toVector

    val surfaceContacts =
      for
        entity  <- entities.filterNot(_.isFixed).toVector
        surface <- surfaces
        if detector.isInside(entity, surface)
      yield SurfaceContact(entity.id, surface.id)

    PhysicsContext(
      state = state,
      dt = dt,
      collisions = CollisionSnapshot(entityContacts, surfaceContacts)
    )
