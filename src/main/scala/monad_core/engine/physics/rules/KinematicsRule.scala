package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.{
  PhysicsDomainError,
  PhysicsError,
  PhysicsRule,
  PhysicsRuleResult
}
import monad_core.engine.physics.utils.{PhysicsUtil, Rotation, SceneEntitiesUpdate}

private[physics] object KinematicsRule:
  private val Id = "kinematics"

  given kinematicsRule: PhysicsRule with

    override val RuleId: String = KinematicsRule.Id

    override def apply(scene: State, dt: Long)(using
        detector: CollisionDetector
    ): Either[PhysicsError, PhysicsRuleResult] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities.filterNot(_.isFixed)

        updatedEntities <- applyKinematics(scene, entities, dt)

        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield PhysicsRuleResult(updatedScene)

  private def applyKinematics(
      scene: State,
      entities: List[Entity],
      dt: Long
  ): Either[PhysicsError, List[Entity]] =
    entities.foldLeft(Right(List.empty[Entity]): Either[PhysicsError, List[Entity]]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntities), entity) =>
        moveEntity(scene, entity, dt).map(updatedEntities :+ _)
    }

  private[physics] def moveEntity(
      scene: State,
      entity: Entity,
      dt: Long
  ): Either[PhysicsError, Entity] =
    for
      seconds <- PhysicsUtil.timeLongToSeconds(dt)
      movedEntity <- entity.speed match
        case Some(speed) =>
          PhysicsUtil
            .nextPosition(entity.position, speed, dt)
            .map(entity.moveTo)
        case None => Right(entity)
      rotatedEntity <- entity.angularSpeed match
        case Some(angularSpeed) =>
          movedEntity
            .rotateTo(Rotation.normalize(entity.rotation + angularSpeed * seconds))
            .left
            .map(PhysicsDomainError.apply)
        case None => Right(movedEntity)
    yield rotatedEntity
