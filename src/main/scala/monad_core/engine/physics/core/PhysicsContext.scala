package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, LocatableId, Surface}

final case class EntityCollisionContact(
    firstId: LocatableId,
    secondId: LocatableId,
    collision: Collision
)

final case class SurfaceContact(
    entityId: LocatableId,
    surfaceId: LocatableId
)

final case class CollisionSnapshot(
    entityContacts: Vector[EntityCollisionContact] = Vector.empty,
    surfaceContacts: Vector[SurfaceContact] = Vector.empty
)

final case class PhysicsContext(
    state: State,
    dt: Long,
    collisions: CollisionSnapshot = CollisionSnapshot()
)

object PhysicsContext:
  private val CombinationSize = 2

  def getEntityMapById(context: PhysicsContext): Map[LocatableId, Entity] =
    context.state.allEntities.map(entity => entity.id -> entity).toMap

  def getSurfaceMapById(context: PhysicsContext): Map[LocatableId, Surface] =
    context.state.allSurfaces.map(surface => surface.id -> surface).toMap

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
