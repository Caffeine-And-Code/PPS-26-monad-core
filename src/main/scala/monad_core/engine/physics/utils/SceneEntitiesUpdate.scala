package monad_core.engine.physics.utils

import monad_core.engine.core.traits.State
import monad_core.engine.model.Entity
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError}

/** Immutable scene-update operations used by physics rules. */
private[physics] object SceneEntitiesUpdate:

  /**
   * Replaces the supplied entities in a scene.
   *
   * @param scene
   *   state to update
   * @param updatedEntities
   *   entity values that replace their existing versions
   * @return
   *   updated state, or the first physics-domain error
   */
  def apply(
      scene: State,
      updatedEntities: List[Entity]
  ): Either[PhysicsError, State] =
    updateSceneWithEntities(scene, updatedEntities)

  /**
   * Applies every entity replacement from left to right.
   *
   * @param scene
   *   initial state
   * @param updatedEntities
   *   ordered entity replacements
   * @return
   *   final state, or the first physics-domain error
   */
  private def updateSceneWithEntities(
      scene: State,
      updatedEntities: List[Entity]
  ): Either[PhysicsError, State] =
    updatedEntities.foldLeft(Right(scene): Either[PhysicsError, State]) { (currentScene, entity) =>
      currentScene.flatMap { s =>
        updateEntity(s, entity)
      }
    }

  /**
   * Replaces one entity through the public immutable state operations.
   *
   * @param scene
   *   state containing the previous entity value
   * @param updatedEntity
   *   replacement entity value
   * @return
   *   updated state, or a wrapped remove/add domain error
   */
  private def updateEntity(
      scene: State,
      updatedEntity: Entity
  ): Either[PhysicsError, State] =
    for
      sceneWithoutEntity <- scene
        .removeEntity(updatedEntity)
        .left
        .map(err => PhysicsDomainError(err))

      updatedScene <- sceneWithoutEntity
        .addEntity(updatedEntity)
        .left
        .map(err => PhysicsDomainError(err))
    yield updatedScene
