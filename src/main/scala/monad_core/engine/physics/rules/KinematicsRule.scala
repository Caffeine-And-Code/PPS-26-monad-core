package monad_core.engine.physics.rules

import monad_core.engine.core.traits.State
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.{
  PhysicsDomainError,
  PhysicsContext,
  PhysicsError,
  PhysicsRule,
  PhysicsRuleResult
}
import monad_core.engine.physics.utils.{PhysicsUtil, Rotation, SceneEntitiesUpdate}

private[physics] object KinematicsRule:
  private val Id = "kinematics"

  given kinematicsRule: PhysicsRule with

    override val RuleId: String = KinematicsRule.Id

    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        entities = context.state.allEntities.filterNot(_.isFixed)

        updatedEntities <- applyKinematics(context.state, entities, context.dt)

        updatedScene <- SceneEntitiesUpdate(context.state, updatedEntities)
      yield PhysicsRuleResult(updatedScene)

  private def applyKinematics(
      state: State,
      entities: List[Entity],
      dt: Long
  ): Either[PhysicsError, List[Entity]] =
    entities.foldLeft(Right(List.empty[Entity]): Either[PhysicsError, List[Entity]]) {
      case (Left(err), _) => Left(err)
      case (Right(updatedEntities), entity) =>
        moveEntity(state, entity, dt).map(updatedEntities :+ _)
    }

  private[physics] def moveEntity(
      state: State,
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
