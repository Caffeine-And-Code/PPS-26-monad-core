package monad_core.engine.physics.rules

import monad_core.engine.model.*
import monad_core.engine.core.traits.State
import monad_core.engine.physics.core.*
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

private[physics] object DamageApplicationRule:
  private val Id = "damage-application"

  given damageApplicationRule: PhysicsRule with

    override val RuleId: String = DamageApplicationRule.Id

    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        _            <- PhysicsUtil.timeLongToSeconds(context.dt)
        updatedScene <- applyDamage(context)
      yield PhysicsRuleResult(updatedScene)

  private def applyDamage(context: PhysicsContext): Either[PhysicsError, State] =
    val originalById = PhysicsContext.getEntityMapById(context)
    val surfacesById = PhysicsContext.getSurfaceMapById(context)

    for
      entitiesAfterCollisions <- context.collisions.entityContacts.foldLeft(
        Right(originalById): Either[PhysicsError, Map[LocatableId, Entity]]
      )(applyEntityDamage)
      entitiesAfterSurfaces <- context.collisions.surfaceContacts.foldLeft(
        Right(entitiesAfterCollisions): Either[PhysicsError, Map[LocatableId, Entity]]
      )((entities, contact) => applySurfaceDamage(entities, surfacesById, contact))
      updatedState <- updateState(context.state, originalById, entitiesAfterSurfaces)
    yield updatedState

  private def applyEntityDamage(
      entities: Either[PhysicsError, Map[LocatableId, Entity]],
      contact: EntityCollisionContact
  ): Either[PhysicsError, Map[LocatableId, Entity]] =
    entities.flatMap { currentById =>
      (currentById.get(contact.firstId), currentById.get(contact.secondId)) match
        case (Some(first), Some(second)) =>
          for
            damagedFirst  <- applyDamage(first, second.damage)
            damagedSecond <- applyDamage(second, first.damage)
          yield updateEntity(
            updateEntity(currentById, first.id, damagedFirst),
            second.id,
            damagedSecond
          )
        case _ => Right(currentById)
    }

  private def applySurfaceDamage(
      entities: Either[PhysicsError, Map[LocatableId, Entity]],
      surfacesById: Map[LocatableId, Surface],
      contact: SurfaceContact
  ): Either[PhysicsError, Map[LocatableId, Entity]] =
    entities.flatMap { currentById =>
      (currentById.get(contact.entityId), surfacesById.get(contact.surfaceId)) match
        case (Some(entity), Some(surface)) =>
          applyDamage(entity, surface.damageOverTime)
            .map(updateEntity(currentById, entity.id, _))
        case _ => Right(currentById)
    }

  private def applyDamage(
      entity: Entity,
      damage: Option[Damage]
  ): Either[PhysicsError, Option[Entity]] =
    (entity.health, damage) match
      case (Some(_), Some(value)) =>
        entity.applyDamage(value.value) match
          case Right(updated)                        => Right(Some(updated))
          case Left(HealthCannotBeNegativeOrZero(_)) => Right(None)
          case Left(error)                           => Left(PhysicsDomainError(error))
      case _ => Right(Some(entity))

  private def updateEntity(
      entities: Map[LocatableId, Entity],
      entityId: LocatableId,
      updated: Option[Entity]
  ): Map[LocatableId, Entity] =
    updated.fold(entities - entityId)(entity => entities.updated(entityId, entity))

  private def updateState(
      state: State,
      originalById: Map[LocatableId, Entity],
      finalById: Map[LocatableId, Entity]
  ): Either[PhysicsError, State] =
    val updatedEntities = finalById.values
      .filter(entity => originalById.get(entity.id).exists(_ != entity))
      .toList
      .sortBy(_.id.value)
    val removedEntities = originalById.values
      .filterNot(entity => finalById.contains(entity.id))
      .toList
      .sortBy(_.id.value)

    for
      updatedState <- SceneEntitiesUpdate(state, updatedEntities)
      finalState <- removedEntities.foldLeft(
        Right(updatedState): Either[PhysicsError, State]
      )((result, entity) =>
        result.flatMap(
          _.removeEntity(entity).left
            .map(PhysicsDomainError.apply)
        )
      )
    yield finalState
