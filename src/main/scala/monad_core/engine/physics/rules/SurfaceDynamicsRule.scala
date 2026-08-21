package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.{+, Entity, Surface, Vector2D}
import monad_core.engine.physics.core.{
  PhysicsError,
  PhysicsRule
}
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
