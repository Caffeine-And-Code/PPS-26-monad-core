package monad_core.engine.physics.rules

import monad_core.engine.model.*
import monad_core.engine.core.traits.State
import monad_core.engine.physics.core.*
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

/**
 * The PhysicsRule which will handle and apply the damage behavior
 * for all the entities and the surfaces in the given state.
 */
private[physics] object DamageApplicationRule:
  private val Id = "damage-application"

  given damageApplicationRule: PhysicsRule with

    override val RuleId: String = DamageApplicationRule.Id

    /**
     * entry point of the rule, it will validate the provided dt and apply the damage to the entities according
     * to the collisions provided in the context.
     * @see [[PhysicsRuleResult]], [[orchestrateDamageApplication]] and [[PhysicsUtil.timeLongToSeconds]]
     *
     * @param context the execution context of the rule
     * @return Left(PhysicsError) if any errors occurs, Right(PhysicsRuleResult) otherwise
     */
    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        _            <- PhysicsUtil.timeLongToSeconds(context.dt)
        updatedScene <- orchestrateDamageApplication(context)
      yield PhysicsRuleResult(updatedScene)

  /**
   * Damage applier orchestrator, it will iterate each collision and execute the application-specific logic
   * to the entity/entities depending on the collision nature.
   * @see [[applyEntityDamage]] [[applySurfaceDamage]]
   * 
   * @param context the execution context of the rule
   * @return Left(PhysicsError) if the specific damage application returns it, 
   *         
   *         Right(state), which will have all the updates/removal of entities, otherwise  
   */
  private def orchestrateDamageApplication(context: PhysicsContext): Either[PhysicsError, State] =
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

  /**
   * Applies the damage to both the entities involved in the contact provided.
   * 
   * @see [[applyDamage]]
   * @param entities all the entities in the state
   * @param contact the collision between two entities in the state
   * @return Left(PhysicsError) only when the application of the damage generates one,
   *         
   *         Right(Map[LocatableId, Entity]), which represents the new entities map for the new state, otherwise 
   */
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

  /**
   * Applies the damage over time (DOT) of the surface to the entity which is colliding to it
   * 
   * @see [[applyDamage]]
   * @param entities the state entities map
   * @param surfacesById the state surface map 
   * @param contact the collision between the entity and the surface
   * @return Left(PhysicsError) only when the application of the damage generates one,
   *         
   *         Right(Map[LocatableId, Entity]), which represents the new entities map for the new state, otherwise
   */
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

  /**
   * propagates and wraps the damage application to the entity method, which will actually apply the damage to the entity and return a copy.
   * 
   * @see [[Entity.applyDamage]]
   * @param entity the entity which the damage will be applied to
   * @param damage the inflicted damage
   * @return Left(PhysicsError) when the applyDamage on the entity returns once, EXCEPT the
   *              HealthCannotBeNegativeOrZero which needs to be handled to signal the removal of the entity itself.
   *              
   *         Right(Option[Entity]) when the damage is applied correctly, if the entity is present the entity needs 
   *         to be updated marking it as "survived", if the Option is None the entity is dead and needs to be removed.
   */
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

  /**
   * It updates the entities map by providing a copy of it.
   * 
   * @param entities old map which needs to be updated 
   * @param entityId the entity id that has been updated
   * @param updated the entity that has been updated
   * @return
   */
  private def updateEntity(
      entities: Map[LocatableId, Entity],
      entityId: LocatableId,
      updated: Option[Entity]
  ): Map[LocatableId, Entity] =
    updated.fold(entities - entityId)(entity => entities.updated(entityId, entity))

  /**
   * It will generate the updated version of the state that then will be returned by [[orchestrateDamageApplication]].
   * 
   * @see [[SceneEntitiesUpdate]] and [[State.removeEntity]]
   * @param state old state
   * @param originalById state map of the entities before each damage update - which will be the map of the state given to [[orchestrateDamageApplication]]
   * @param finalById state map of the entities after each damage update
   * @return
   */
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
