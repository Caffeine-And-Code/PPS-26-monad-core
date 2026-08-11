package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule}
import monad_core.engine.physics.pathfinding.SizeHelper
import monad_core.engine.physics.utils.*

private[physics] object BorderContactRule:
  private val Id = "border-contact"

  given borderContactRule: PhysicsRule with

    override val RuleId: String = BorderContactRule.Id

    override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities.filterNot(_.isFixed)

        activeCollisions <- findCollisions(entities, scene.UpperLeftCorner, scene.LowerRightCorner)
          .left.map(PhysicsDomainError.apply)

        updatedEntities <- CollisionResolver(activeCollisions)

        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield updatedScene
      
    private def findCollisions(
      entities: List[Entity], 
      upperLeft: Vector2D, 
      lowerRight: Vector2D
    ) (using detector: CollisionDetector) : Either[EngineError, CollisionMap] = {

      entities.foldLeft(Right(Map.empty): Either[EngineError, CollisionMap]) {
        case (acc, entity) =>
          for
            collisions <- collisionWithBorder(entity, upperLeft, lowerRight)
            result <- acc.map(_.updated(entity, collisions))
          yield result
      }
    }

    private def collisionWithBorder(
         entity: Entity, 
         upperLeft: Vector2D, 
         lowerRight: Vector2D
       ) (using detector: CollisionDetector)
        : Either[EngineError, List[(Entity, Collision)]] =

      val position = entity.position
      val horizontal = SizeHelper.horizontalShapeSize(entity) / 2
      val vertical = SizeHelper.verticalShapeSize(entity) / 2

      val borderTypes =
        List(
          Option.when(position.x - horizontal < upperLeft.x)(BorderWallType.Left),
          Option.when(position.x + horizontal > lowerRight.x)(BorderWallType.Right),
          Option.when(position.y - vertical < upperLeft.y)(BorderWallType.Top),
          Option.when(position.y + vertical > lowerRight.y)(BorderWallType.Bottom)
        ).flatten

      borderTypes.foldLeft(
          Right(List.empty[(Entity, Collision)]): Either[EngineError, List[(Entity, Collision)]]
        ) {
          case (Left(error), _) =>
            Left(error)

          case (Right(walls), borderType) =>
            BorderWall(
              entity,
              horizontal,
              vertical,
              upperLeft,
              lowerRight,
              borderType
            ).map(wall => walls :+ wall)
        }
