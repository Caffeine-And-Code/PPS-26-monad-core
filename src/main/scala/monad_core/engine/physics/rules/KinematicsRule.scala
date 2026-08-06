package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneUpdateEntity}

private[physics] object KinematicsRule:

  private val id = "kinematics"

  given kinematicsRule: PhysicsRule with

    override val ruleId: String = KinematicsRule.id

    override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)
        entities = scene.allEntities.filterNot(_.isFixed)

        updatedEntities <- applyKinematics(scene, entities, dt)

        updatedScene <- SceneUpdateEntity.updateEntities(scene, updatedEntities)

      yield updatedScene

    private def applyKinematics(scene: State, entities: List[Entity], dt: Long): Either[PhysicsError, List[Entity]] =
      entities.foldLeft(Right(List.empty[Entity]): Either[PhysicsError, List[Entity]]) {
        case (Left(err), _) => Left(err)
        case (Right(updatedEntities), entity) =>
          moveEntity(scene, entity, dt).map(updatedEntities :+ _)
      }

    private def moveEntity(scene: State, entity: Entity, dt: Long): Either[PhysicsError, Entity] =
      entity.speed match
        case Some(speed) =>
          val nextPos = PhysicsUtil.nextPosition(entity.position, speed, dt, scene.UpperLeftCorner, scene.LowerRightCorner)
          nextPos match {
            case Right(pos) => entity.moveTo(pos).left.map(PhysicsDomainError.apply)
            case Left(err) => Left(err)
          }
        case None => Right(entity)


