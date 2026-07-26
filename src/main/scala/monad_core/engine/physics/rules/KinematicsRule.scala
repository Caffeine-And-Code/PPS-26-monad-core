package monad_core.engine.physics.rules

import monad_core.engine.physics.core.{OutOfBoundEntity, PhysicsError, PhysicsRule, PhysicsState, PhysicsUtil}

object KinematicsRule:

  private val id = "kinematics"
  
  given kinematicsRule[S, CD](using state: PhysicsState[S]): PhysicsRule[S, CD] with
    
    override val ruleId: String = KinematicsRule.id
    
    override def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)
        entities = state.getEntities(scene)

        updatedScene <- entities.foldLeft[Either[PhysicsError, S]](Right(scene)):
          case (Left(err), _) => Left(err)
          case (Right(currentScene), (entityId, entity)) =>
            entity.speed match
              case None => Right(currentScene)
              case Some(speed) =>
                for
                  nextPos <- PhysicsUtil.nextPosition(entity.position, speed, dt)
                  moved   <- entity.moveTo(nextPos).left.map(_ => OutOfBoundEntity(nextPos))
                yield state.updateEntity(currentScene, entityId, moved)
      yield updatedScene