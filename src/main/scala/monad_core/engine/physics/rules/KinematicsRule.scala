package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule, PhysicsRuleError}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

private[physics] object KinematicsRule:
  private val Id = "kinematics"

  given kinematicsRule: PhysicsRule with

    override val RuleId: String = KinematicsRule.Id

    override def apply(scene: State, dt: Long)(using
        detector: CollisionDetector
    ): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities.filterNot(_.isFixed)

        updatedEntities <- applyKinematics(scene, entities, dt)

        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield updatedScene

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
      speed <- entity.speed match
        case Some(s) => Right(s)
        case None =>
          Left(PhysicsRuleError(s"Entity ${entity.id} has no speed to apply kinematics"))

      newPosition <- PhysicsUtil.nextPosition(
        entity.position,
        speed,
        dt,
        scene.bounds.upperLeft,
        scene.bounds.lowerRight
      ) match
        case Right(pos) => Right(entity.moveTo(pos))
        case Left(err)  => Left(err)
    yield newPosition
