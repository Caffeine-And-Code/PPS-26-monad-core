package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.Event.EntityCollisionDetectedEvent
import monad_core.engine.core.traits.State
import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule, PhysicsRuleResult}
import monad_core.engine.physics.utils.{
  CollisionMap,
  CollisionResolver,
  PhysicsUtil,
  SceneEntitiesUpdate
}

private[physics] object CollisionResolutionRule:
  private val Id              = "collision-resolution"
  private val CombinationSize = 2

  final private case class DetectedCollision(
      first: Entity,
      second: Entity,
      collision: Collision
  )

  given collisionResolutionRule: PhysicsRule with

    override val RuleId: String = CollisionResolutionRule.Id

    override def apply(scene: State, dt: Long)(using
        detector: CollisionDetector
    ): Either[PhysicsError, PhysicsRuleResult] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities

        detectedCollisions = findCollisions(entities)
        activeCollisions   = toCollisionMap(detectedCollisions)

        updatedEntities <- CollisionResolver(activeCollisions)

        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield PhysicsRuleResult(
        state = updatedScene,
        events = detectedCollisions.map(toEvent)
      )

    private def findCollisions(
        entities: List[Entity]
    )(using detector: CollisionDetector): Vector[DetectedCollision] =
      entities
        .combinations(CollisionResolutionRule.CombinationSize)
        .collect {
          case Seq(e1, e2) if !(e1.isFixed && e2.isFixed) =>
            detector
              .collision(e1, e2)
              .map(DetectedCollision(e1, e2, _))
        }
        .flatten
        .toVector

    private def toCollisionMap(collisions: Vector[DetectedCollision]): CollisionMap =
      collisions
        .flatMap { detected =>
          Vector(
            detected.first -> (detected.second, detected.collision),
            detected.second -> (
              detected.first,
              detected.collision.copy(normalVector = detected.collision.normalVector.flip)
            )
          )
        }
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toList)
        .toMap

    private def toEvent(detected: DetectedCollision): EntityCollisionDetectedEvent =
      EntityCollisionDetectedEvent(
        entityId = detected.first.id,
        modelCollidedWith = detected.second,
        collisionData = detected.collision
      )
