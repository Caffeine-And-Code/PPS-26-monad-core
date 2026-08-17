package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.{+, Entity, Surface}
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule, PhysicsRuleError}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

private[physics] object SurfaceDynamicsRule:

  private val Id = "surface-dynamics"

  given surfaceDynamicsRule: PhysicsRule with

    override val RuleId: String = SurfaceDynamicsRule.Id

    override def apply(scene: State, dt: Long)(using
        collisionDetector: CollisionDetector
    ): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities.filterNot(_.isFixed)
        surfaces = scene.allSurfaces

        entitiesInsideSurfaces = findEntitiesInsideSurfaces(entities, surfaces)

        updatedEntities <- applySurfacesToEntities(entitiesInsideSurfaces, dt)

        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield updatedScene

  private def findEntitiesInsideSurfaces(entities: List[Entity], surfaces: List[Surface])(using
      collisionDetector: CollisionDetector
  ): Seq[(Entity, Surface)] =
    for
      entity  <- entities
      surface <- surfaces
      if collisionDetector.isInside(entity, surface)
    yield (entity, surface)

  private def applySurfacesToEntities(
      containing: Seq[(Entity, Surface)],
      dt: Long
  ): Either[PhysicsError, List[Entity]] =
    containing.foldLeft(Right(List.empty[Entity]): Either[PhysicsError, List[Entity]]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntities), (entity, surface)) =>
        applySurfaceDynamics(entity, surface, dt).map { updatedEntity =>
          updatedEntities :+ updatedEntity
        }
    }

  private def applySurfaceDynamics(
      entity: Entity,
      surface: Surface,
      dt: Long
  ): Either[PhysicsError, Entity] =
    for
      speed <- entity.speed match
        case Some(s) => Right(s)
        case None    => Left(PhysicsRuleError(s"Entity ${entity.id} is fixed, it cannot be applied surface dynamics"))

      speedAfterForce <-
        surface.appliedForce match
          case Some(force) =>
            PhysicsUtil
              .acceleration(force, entity.weight)
              .map(acceleration => speed + acceleration)
          case _ =>
            Right(speed)

      speedAfterFriction <-
        surface.frictionIndex match
          case Some(friction) =>
            PhysicsUtil
              .applyFriction(speedAfterForce, friction, dt)
              .left
              .map(PhysicsDomainError.apply)
          case _ =>
            Right(speedAfterForce)

      updatedEntity = entity.withSpeed(speedAfterFriction)
    yield updatedEntity
