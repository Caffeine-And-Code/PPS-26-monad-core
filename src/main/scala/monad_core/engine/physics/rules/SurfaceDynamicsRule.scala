package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.{Entity, Surface, Vector2D}
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule}
import monad_core.engine.physics.utils.PhysicsUtil
import monad_core.engine.physics.utils.SceneUpdateEntity

private[physics] object SurfaceDynamicsRule:

  private val id = "surface-dynamics"

  given surfaceDynamicsRule: PhysicsRule with

    override val ruleId: String = SurfaceDynamicsRule.id

    override def apply(scene: State, dt: Long)(using collisionDetector: CollisionDetector): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)
        entities = scene.allEntities.filterNot(_.isFixed)
        surfaces = scene.allSurfaces

        entitiesInsideSurfaces = findEntitiesInsideSurfaces(entities, surfaces)

        updatedEntities <- applySurfacesToEntities(entitiesInsideSurfaces, dt)

        updatedScene <- SceneUpdateEntity.updateEntities(scene, updatedEntities)
      yield updatedScene

  private def findEntitiesInsideSurfaces(entities: List[Entity], surfaces: List[Surface])(using collisionDetector: CollisionDetector): Seq[(Entity, Surface)] =
    for
      entity <- entities
      surface <- surfaces
      if collisionDetector.isInside(entity, surface)
    yield (entity, surface)

  private def applySurfacesToEntities(containing: Seq[(Entity, Surface)], dt: Long): Either[PhysicsError, List[Entity]] =
    containing.foldLeft(Right(List.empty[Entity]): Either[PhysicsError, List[Entity]]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntities), (entity, surface)) =>
        applySurfaceDynamics(entity, surface, dt).map { updatedEntity =>
          updatedEntities :+ updatedEntity
        }
    }

  private def applySurfaceDynamics(entity: Entity, surface: Surface, dt: Long): Either[PhysicsError, Entity] =
    entity.speed match
      case None =>
        Right(entity)

      case Some(speed) =>
        for
          speedAfterForce <-
            surface.appliedForce match
              case Some(force) =>
                PhysicsUtil
                  .acceleration(force, entity.weight)
                  .left
                  .map(PhysicsDomainError.apply)
                  .flatMap(acc => PhysicsUtil.nextSpeed(speed, acc, dt))
              case _ =>
                Right(speed)

          speedAfterFriction <-
            surface.frictionIndex.fold[Either[PhysicsError, Vector2D]](Right(speedAfterForce)) { friction =>
              PhysicsUtil.applyFriction(speedAfterForce, friction, dt)
            }

          updatedEntity <- entity
            .withSpeed(speedAfterFriction)
            .left
            .map(PhysicsDomainError.apply)
        yield updatedEntity