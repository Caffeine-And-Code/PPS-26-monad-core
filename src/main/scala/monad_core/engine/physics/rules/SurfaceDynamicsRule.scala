package monad_core.engine.physics.rules

import monad_core.engine.model.{+, Entity, LocatableId, Surface, Vector2D}
import monad_core.engine.physics.core.{PhysicsContext, PhysicsError, PhysicsRule, PhysicsRuleResult}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

private[physics] object SurfaceDynamicsRule:

  private val Id = "surface-dynamics"

  given surfaceDynamicsRule: PhysicsRule with

    override val RuleId: String = SurfaceDynamicsRule.Id

    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        updatedEntities <- applySurfacesToEntities(context)

        updatedScene <- SceneEntitiesUpdate(context.state, updatedEntities)
      yield PhysicsRuleResult(updatedScene)

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

  private def applyForce(
      entity: Entity,
      surface: Surface
  ): Either[PhysicsError, Entity] =
    (entity.speed, surface.appliedForce) match
      case (Some(speed), Some(force)) =>
        PhysicsUtil
          .acceleration(force, entity.weight)
          .map(acceleration => entity.withSpeed(speed + acceleration))
      case _ =>
        Right(entity)

  private def applyFriction(
      entity: Entity,
      surface: Surface,
      dt: Long
  ): Either[PhysicsError, Entity] =
    (entity.speed, surface.frictionIndex) match
      case (Some(speed), Some(friction)) =>
        PhysicsUtil
          .applyFriction(speed, friction, dt)
          .map(entity.withSpeed)
      case _ => Right(entity)

  private def applyAngular(
      entity: Entity,
      surface: Surface,
      dt: Long
  ): Either[PhysicsError, Entity] =
    (entity.angularSpeed, surface.frictionIndex) match
      case (Some(angularSpeed), Some(friction)) =>
        PhysicsUtil
          .applyAngularFriction(angularSpeed, friction, dt)
          .map(entity.withAngularSpeed)
      case _ => Right(entity)
