package monad_core.engine.physics.rules

import monad_core.engine.core.events.EngineEvent.CollisionDetected
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.geometry.Collision
import monad_core.engine.model.{BorderSide, EngineError, Entity, Vector2D}
import monad_core.engine.physics.core.{
  PhysicsDomainError,
  PhysicsContext,
  PhysicsError,
  PhysicsRule,
  PhysicsRuleResult
}
import monad_core.engine.physics.pathfinding.SizeHelper
import monad_core.engine.physics.utils.*

/** Physics rule that detects and resolves contacts with scene borders. */
private[physics] object BorderContactRule:
  /** Stable identifier of the border-contact rule. */
  private val Id = "border-contact"

  /**
   * Collision between a movable entity and a generated border wall.
   *
   * @param entity
   *   entity crossing the border
   * @param wall
   *   fixed wall representing the crossed border
   * @param side
   *   side of the scene involved in the collision
   * @param collision
   *   geometric collision data
   */
  final private case class DetectedBorderCollision(
      entity: Entity,
      wall: Entity,
      side: BorderSide,
      collision: Collision
  )

  /** Default border-contact physics rule. */
  given borderContactRule: PhysicsRule with

    override val RuleId: String = BorderContactRule.Id

    /**
     * Resolves every movable entity contacting a scene border.
     *
     * @param context
     *   physics context containing the current state and elapsed time
     * @return
     *   updated state and border-collision events, or a [[PhysicsError]]
     */
    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(context.dt)
        entities = context.state.allEntities.filterNot(_.isFixed)

        detectedCollisions <- findCollisions(
          entities,
          context.state.bounds.upperLeft,
          context.state.bounds.lowerRight
        ).left.map(PhysicsDomainError.apply)
        activeCollisions = toCollisionMap(entities, detectedCollisions)

        updatedEntities <- CollisionResolver(activeCollisions)

        updatedScene <- SceneEntitiesUpdate(context.state, updatedEntities)
      yield PhysicsRuleResult(
        state = updatedScene,
        events = detectedCollisions.map(toEvent)
      )

    /**
     * Finds every border crossed by the supplied entities.
     *
     * @param entities
     *   movable entities to inspect
     * @param upperLeft
     *   upper-left corner of the scene bounds
     * @param lowerRight
     *   lower-right corner of the scene bounds
     * @return
     *   detected border collisions, or the first [[EngineError]]
     */
    private def findCollisions(
        entities: List[Entity],
        upperLeft: Vector2D,
        lowerRight: Vector2D
    ): Either[EngineError, Vector[DetectedBorderCollision]] =
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

    /**
     * Groups detected border collisions by movable entity.
     *
     * @param entities
     *   entities that must appear in the resulting map
     * @param collisions
     *   detected border collisions
     * @return
     *   collision map consumed by the resolver
     */
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

    /**
     * Converts a detected border collision into an engine event.
     *
     * @param detected
     *   border collision to convert
     * @return
     *   corresponding collision event
     */
    private def toEvent(detected: DetectedBorderCollision): CollisionDetected =
      CollisionDetected(
        entityId = detected.entity.id,
        target = CollisionTarget.Border(detected.side),
        collision = detected.collision
      )

    /**
     * Detects which bounds are crossed by an entity and builds their collision walls.
     * Half sizes place each comparison on the entity edge.
     *
     * @param entity
     *   entity to inspect
     * @param upperLeft
     *   upper-left corner of the scene bounds
     * @param lowerRight
     *   lower-right corner of the scene bounds
     * @return
     *   border sides, generated walls and collision data, or a [[EngineError]]
     */
    private def collisionWithBorder(
        entity: Entity,
        upperLeft: Vector2D,
        lowerRight: Vector2D
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
          ).map { case BorderWallResult(wall, collision) =>
            walls :+ (borderSide, wall, collision)
          }
      }
