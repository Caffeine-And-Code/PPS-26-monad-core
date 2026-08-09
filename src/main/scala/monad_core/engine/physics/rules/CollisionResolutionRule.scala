package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule}
import monad_core.engine.physics.utils.{CollisionMap, CollisionResolver, PhysicsUtil, SceneEntitiesUpdate}

private[physics] object CollisionResolutionRule:
  private val id = "collision-resolution"

  given collisionResolutionRule: PhysicsRule with

    override val ruleId: String = CollisionResolutionRule.id

    override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)
        entities = scene.allEntities
        
        activeCollisions = findCollisions(entities)
        
        updatedEntities <- CollisionResolver(activeCollisions)

        updatedScene <-
          if activeCollisions.isEmpty then
            Right(scene)
          else
            SceneEntitiesUpdate(scene, updatedEntities)

      yield updatedScene

    private def findCollisions(
                                entities: List[Entity]
                              )(using detector: CollisionDetector): CollisionMap =
      entities
        .combinations(2)
        .collect {
          case Seq(e1, e2)
            if !(e1.isFixed && e2.isFixed) =>
            collisionBetween(e1, e2)
        }
        .flatten
        .toList
        .groupMap(_._1)(_._2)

    private def collisionBetween(
                                  e1: Entity,
                                  e2: Entity
                                )(using detector: CollisionDetector): List[(Entity, (Entity, Collision))] =
      detector.collision(e1, e2).toList.flatMap { collision =>
        List(
          e1 -> (e2, collision),
          e2 -> (e1, collision.copy(normalVector = collision.normalVector.flip))
        )
      }