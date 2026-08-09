package monad_core.engine.physics.utils

import monad_core.engine.core.traits.State
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRuleError}

private[physics] object SceneEntitiesUpdate:

  def apply(
                      scene: State,
                      updatedEntities: List[Entity]
                    ): Either[PhysicsError, State] =
    updatedEntities.foldLeft(Right(scene): Either[PhysicsError, State]) {
      (currentScene, entity) =>
        currentScene.flatMap { s =>
          updateEntity(s, entity)
        }
    }

  private def updateEntity(
                            scene: State,
                            updatedEntity: Entity
                          ): Either[PhysicsError, State] =
    for
      sceneWithoutEntity <- scene
        .removeEntity(updatedEntity)
        .left.map(err => PhysicsDomainError(err))

      updatedScene <- sceneWithoutEntity
        .addEntity(updatedEntity)
        .left.map(err => PhysicsDomainError(err))
    yield updatedScene