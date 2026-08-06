package monad_core.engine.physics.utils

import monad_core.engine.core.traits.State
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.{PhysicsError, PhysicsRuleError}

private[physics] object SceneUpdateEntity:

  def updateEntities(
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
        .left.map(err => PhysicsRuleError(err.toString))

      updatedScene <- sceneWithoutEntity
        .addEntity(updatedEntity)
        .left.map(err => PhysicsRuleError(err.toString))
    yield updatedScene