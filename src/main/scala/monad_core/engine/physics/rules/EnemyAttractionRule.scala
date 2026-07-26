package monad_core.engine.physics.rules

import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule, PhysicsState, PhysicsUtil}

object EnemyAttractionRule:
  
  private val id = "enemy-attraction"
  private val AttractionAcceleration = 1.0

  given enemyAttractionRule[S, CD](using state: PhysicsState[S]): PhysicsRule[S, CD] with
    
    override val ruleId: String = EnemyAttractionRule.id
    
    override def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)

        entities = state.getEntities(scene)
        teams = state.getTeams(scene)

        updatedScene <- entities.foldLeft[Either[PhysicsError, S]](Right(scene)):
          case (Left(err), _) => Left(err)
          case (Right(currentScene), (entityId, entity)) =>
            val maybeUpdatedScene: Option[Either[PhysicsError, S]] = for
              enemy     <- PhysicsUtil.nearestEnemy(entity, entities, teams)
              speed     <- entity.speed
              direction <- PhysicsUtil.direction(entity.position, enemy.position)
            yield
              for
                nextSpeed     <- PhysicsUtil.nextSpeed(speed, direction * AttractionAcceleration, dt)
                updatedEntity <- entity.withSpeed(nextSpeed).left.map(PhysicsDomainError.apply)
              yield
                state.updateEntity(currentScene, entityId, updatedEntity)
            maybeUpdatedScene.getOrElse(Right(currentScene))
      yield updatedScene