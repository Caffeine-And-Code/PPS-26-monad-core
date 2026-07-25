package engine.physics.rules

import engine.model.*
import engine.physics.core.*

object EnemyAttractionRule:

  private val AttractionAcceleration = 1.0

  given enemyAttractionRule[S, CD](using state: PhysicsState[S]): PhysicsRule[S, CD] with
    override def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)

        entities = state.getEntities(scene)
        teams = state.getTeams(scene)

        updatedScene <-
          entities.foldLeft[Either[PhysicsError, S]](Right(scene)):
            case (sceneResult, (entityId, entity)) =>
              sceneResult.flatMap: currentScene =>
                PhysicsUtil.nearestEnemy(entity, entities, teams) match
                  case None =>
                    Right(currentScene)
                  case Some(enemy) =>
                    entity.speed match
                      case None =>
                        Right(currentScene)

                      case Some(speed) =>
                        PhysicsUtil
                          .direction(entity.position, enemy.position)
                          .fold[Either[PhysicsError, S]](Right(currentScene)):
                            direction =>
                              for
                                nextSpeed <- PhysicsUtil.nextSpeed(
                                  speed = speed,
                                  acceleration = direction * AttractionAcceleration,
                                  deltaTime = dt
                                )
                                updatedEntity <- entity
                                .withSpeed(nextSpeed)
                                .left
                                .map(PhysicsDomainError.apply)
                              yield state.updateEntity(
                                currentScene,
                                entityId,
                                updatedEntity
                              )
      yield updatedScene