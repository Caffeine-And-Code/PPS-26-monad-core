package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, LocatableId, Surface}

/**
 * Record class representing the collision between two entities
 *
 * @see [[Collision]] and [[PhysicsContext.detect]]
 * @param firstId first entity id
 * @param secondId second entity id
 * @param collision collision record produced by the collision system ([[CollisionDetector]])
 */
final case class EntityCollisionContact(
    firstId: LocatableId,
    secondId: LocatableId,
    collision: Collision
)

/**
 * Record class representing the collision between a surface and a entity
 *
 * @see [[PhysicsContext.detect]]
 * @param entityId entity id
 * @param surfaceId surface id
 */
final case class SurfaceContact(
    entityId: LocatableId,
    surfaceId: LocatableId
)

/**
 * Record class representing a specific moment in the simulation by collecting all the collisions,
 * useful for the physic system to not recalculate them each time
 *
 * @see [[PhysicsContext.detect]]
 * @param entityContacts all the collisions between entities
 * @param surfaceContacts all the collisions between an entity and a surface
 */
final case class CollisionSnapshot(
    entityContacts: Vector[EntityCollisionContact] = Vector.empty,
    surfaceContacts: Vector[SurfaceContact] = Vector.empty
)

/**
 * Record class used as input to each rule execution
 *
 * @see [[CollisionSnapshot]], [[State]] and [[PhysicsRule]]
 * @param state the state before the rule execution
 * @param dt the delta time, representing the time difference between a prior tick and the current tick
 * @param collisions the collision snapshot
 */
final case class PhysicsContext(
    state: State,
    dt: Long,
    collisions: CollisionSnapshot = CollisionSnapshot()
)

object PhysicsContext:
  private val CombinationSize = 2

  /**
   * Format the entities list from the state to a map entity.id -> entity
   *
   * @param context rule context
   * @return the produced map
   */
  def getEntityMapById(context: PhysicsContext): Map[LocatableId, Entity] =
    context.state.allEntities.map(entity => entity.id -> entity).toMap

  /**
   * Format the surface list from the state to a map surface.id -> surface
   *
   * @param context rule context
   * @return the produced map
   */
  def getSurfaceMapById(context: PhysicsContext): Map[LocatableId, Surface] =
    context.state.allSurfaces.map(surface => surface.id -> surface).toMap

  /**
   * Detect all the collisions via the appliance of the [[CollisionDetector]] and produces the
   * initial [[PhysicsContext]] for the first rule to be executed
   *
   * @see [[RuleCombinator]]
   * @param state the state record BEFORE the physics rule appliance
   * @param dt the delta time, representing the time difference between a prior tick and the current tick
   * @param detector the collision detector
   * @return
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
