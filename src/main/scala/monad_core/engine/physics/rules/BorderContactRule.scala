package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.EngineEvent.CollisionDetected
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.core.traits.State
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{BorderSide, EngineError, Entity, Vector2D}
import monad_core.engine.physics.core.{
  PhysicsDomainError,
  PhysicsError,
  PhysicsRule,
  PhysicsRuleResult
}
import monad_core.engine.physics.pathfinding.SizeHelper
import monad_core.engine.physics.utils.*

private[physics] object BorderContactRule:
  private val Id = "border-contact"

  final private case class DetectedBorderCollision(
      entity: Entity,
      wall: Entity,
      side: BorderSide,
      collision: Collision
  )

  given borderContactRule: PhysicsRule with

    override val RuleId: String = BorderContactRule.Id

    override def apply(scene: State, dt: Long)(using
        detector: CollisionDetector
    ): Either[PhysicsError, PhysicsRuleResult] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities.filterNot(_.isFixed)

        detectedCollisions <- findCollisions(
          entities,
          scene.bounds.upperLeft,
          scene.bounds.lowerRight
        ).left.map(PhysicsDomainError.apply)
        activeCollisions = toCollisionMap(entities, detectedCollisions)

        updatedEntities <- CollisionResolver(activeCollisions)

        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield PhysicsRuleResult(
        state = updatedScene,
        events = detectedCollisions.map(toEvent)
      )

    private def findCollisions(
        entities: List[Entity],
        upperLeft: Vector2D,
        lowerRight: Vector2D
    )(using detector: CollisionDetector): Either[EngineError, Vector[DetectedBorderCollision]] =
      entities.foldLeft(
        Right(Vector.empty): Either[EngineError, Vector[DetectedBorderCollision]]
      ) { case (acc, entity) =>
        acc.flatMap { detected =>
          collisionWithBorder(entity, upperLeft, lowerRight).map { collisions =>
            detected ++ collisions.map { case (side, wall, collision) =>
              DetectedBorderCollision(entity, wall, side, collision)
            }
          }
        }
      }

    private def toCollisionMap(
        entities: List[Entity],
        collisions: Vector[DetectedBorderCollision]
    ): CollisionMap =
      entities.map { entity =>
        val entityCollisions = collisions.collect {
          case DetectedBorderCollision(`entity`, wall, _, collision) => wall -> collision
        }.toList
        entity -> entityCollisions
      }.toMap

    private def toEvent(detected: DetectedBorderCollision): CollisionDetected =
      CollisionDetected(
        entityId = detected.entity.id,
        target = CollisionTarget.Border(detected.side),
        collision = detected.collision
      )

    private def collisionWithBorder(
        entity: Entity,
        upperLeft: Vector2D,
        lowerRight: Vector2D
    )(using
        detector: CollisionDetector
    ): Either[EngineError, List[(BorderSide, Entity, Collision)]] =

      val position   = entity.position
      val horizontal = SizeHelper.horizontalShapeSize(entity) / 2
      val vertical   = SizeHelper.verticalShapeSize(entity) / 2

      val borderSides =
        List(
          Option.when(position.x - horizontal < upperLeft.x)(BorderSide.Left),
          Option.when(position.x + horizontal > lowerRight.x)(BorderSide.Right),
          Option.when(position.y - vertical < upperLeft.y)(BorderSide.Top),
          Option.when(position.y + vertical > lowerRight.y)(BorderSide.Bottom)
        ).flatten

      borderSides.foldLeft(
        Right(List.empty[(BorderSide, Entity, Collision)]): Either[
          EngineError,
          List[(BorderSide, Entity, Collision)]
        ]
      ) {
        case (Left(error), _) =>
          Left(error)

        case (Right(walls), borderSide) =>
          BorderWall(
            entity,
            horizontal,
            vertical,
            upperLeft,
            lowerRight,
            borderSide
          ).map { case (wall, collision) =>
            walls :+ (borderSide, wall, collision)
          }
      }
