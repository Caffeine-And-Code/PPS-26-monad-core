package monad_core.engine.core

import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.model.{Entity, Vector2D}

//TODO: Remove this 
object PhysicsMock extends PhysicsEngine:
  override def step(scene: State, dt: Long): State = {
    scene.allEntities.foldLeft(scene) { (currentScene, entity) =>
      val updatedSceneResult = for {
        movedEntity <- entity.moveBy(Vector2D(0, 1))
        sceneWithout <- currentScene.removeEntity(entity)
        sceneWithNew <- sceneWithout.addEntity(movedEntity)
      } yield sceneWithNew

      updatedSceneResult.getOrElse(currentScene)
    }
  }