package monad_core.engine.physics.rules

import monad_core.engine.model.{+, Entity, LocatableId, Surface, Vector2D}
import monad_core.engine.physics.core.{PhysicsContext, PhysicsError, PhysicsRule, PhysicsRuleResult}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

/** Applies forces and friction from contacted surfaces to mobile entities. */
private[physics] object SurfaceDynamicsRule:

  private val Id = "surface-dynamics"

  /** Physics rule instance applying the dynamics of all current surface contacts. */
  given surfaceDynamicsRule: PhysicsRule with

    override val RuleId: String = SurfaceDynamicsRule.Id

    /**
     * Applies surface dynamics and updates the changed scene entities.
     *
     * @param context
     *   current scene, surface contacts, and elapsed time
     * @return
     *   updated physics state, or the first [[PhysicsError]]
     */
    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        updatedEntities <- applySurfacesToEntities(context)

        updatedScene <- SceneEntitiesUpdate(context.state, updatedEntities)
      yield PhysicsRuleResult(updatedScene)

  /**
   * Folds every surface contact into an entity map without duplicating updates.
   *
   * @param context
   *   current scene, surface contacts, and elapsed time
   * @return
   *   entities changed by contacted surfaces, or the first [[PhysicsError]]
   */
  private def applySurfacesToEntities(
      context: PhysicsContext
  ): Either[PhysicsError, List[Entity]] =
    val originalById = PhysicsContext.getEntityMapById(context)
    val surfacesById = PhysicsContext.getSurfaceMapById(context)

    context.collisions.surfaceContacts
      .foldLeft(Right(originalById): Either[PhysicsError, Map[LocatableId, Entity]]) {
        (result, contact) =>
          result.flatMap { currentById =>
            (currentById.get(contact.entityId), surfacesById.get(contact.surfaceId)) match
              case (Some(entity), Some(surface)) =>
                applySurfaceDynamics(entity, surface, context.dt)
                  .map(updated => currentById.updated(updated.id, updated))
              case _ => Right(currentById)
          }
      }
      .map { updatedById =>
        updatedById.values.filter(entity => originalById(entity.id) != entity).toList
      }

  /**
   * Applies force, linear friction, and angular friction from one surface.
   *
   * @param entity
   *   contacted entity
   * @param surface
   *   surface providing dynamics
   * @param dt
   *   elapsed nanoseconds
   * @return
   *   updated entity, or the first [[PhysicsError]]
   */
  private[physics] def applySurfaceDynamics(
      entity: Entity,
      surface: Surface,
      dt: Long
  ): Either[PhysicsError, Entity] =
    for
      entityAfterForce <- applyForce(entity, surface)

      entityAfterSpeedFriction <- applyFriction(entityAfterForce, surface, dt)

      entityAfterAngularFriction <- applyAngular(entityAfterSpeedFriction, surface, dt)
    yield entityAfterAngularFriction

  /**
   * Adds the acceleration produced by an optional surface force.
   *
   * @param entity
   *   contacted entity
   * @param surface
   *   surface providing an optional force
   * @return
   *   entity with updated velocity, or a [[PhysicsError]]
   */
  private def applyForce(
      entity: Entity,
      surface: Surface
  ): Either[PhysicsError, Entity] =
    (entity.speed, surface.appliedForce) match
      case (Some(speed), Some(force)) =>
        PhysicsUtil
          .acceleration(force, entity.weight)
          .map(acceleration => entity.withSpeed(Some(speed + acceleration)))
      case _ =>
        Right(entity)

  /**
   * Applies optional surface friction to linear velocity.
   *
   * @param entity
   *   contacted entity
   * @param surface
   *   surface providing an optional friction index
   * @param dt
   *   elapsed nanoseconds
   * @return
   *   entity with reduced linear velocity, or a [[PhysicsError]]
   */
  private def applyFriction(
      entity: Entity,
      surface: Surface,
      dt: Long
  ): Either[PhysicsError, Entity] =
    (entity.speed, surface.frictionIndex) match
      case (Some(speed), Some(friction)) =>
        PhysicsUtil
          .applyFriction(speed, friction, dt)
          .map(speed => entity.withSpeed(Some(speed)))
      case _ => Right(entity)

  /**
   * Applies optional surface friction to angular velocity.
   *
   * @param entity
   *   contacted entity
   * @param surface
   *   surface providing an optional friction index
   * @param dt
   *   elapsed nanoseconds
   * @return
   *   entity with reduced angular velocity, or a [[PhysicsError]]
   */
  private def applyAngular(
      entity: Entity,
      surface: Surface,
      dt: Long
  ): Either[PhysicsError, Entity] =
    (entity.angularSpeed, surface.frictionIndex) match
      case (Some(angularSpeed), Some(friction)) =>
        PhysicsUtil
          .applyAngularFriction(angularSpeed, friction, dt)
          .map(angularSpeed => entity.withAngularSpeed(Some(angularSpeed)))
      case _ => Right(entity)
