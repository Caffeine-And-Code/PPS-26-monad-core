package monad_core.engine.physics.rules

import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsContext, PhysicsError, PhysicsRule, PhysicsRuleResult}
import monad_core.engine.physics.pathfinding.{RayCast, VertexFinder}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

/** Steers mobile entities towards the nearest visible enemy or a detour waypoint. */
private[physics] object EnemyAttractionRule:
  private val Id          = "enemy-attraction"
  private val MaxTurnRate = 4.0 // radians per second

  /** Physics rule instance applying enemy attraction to the current scene. */
  given enemyAttractionRule: PhysicsRule with

    override val RuleId: String = EnemyAttractionRule.Id

    /**
     * Applies attraction to every non-fixed entity and updates the scene.
     *
     * @param context
     *   current scene, collisions, and elapsed time
     * @return
     *   updated physics state, or the first [[PhysicsError]]
     */
    override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(context.dt)
        entities = context.state.allEntities
        teams    = context.state.allTeams

        vertexes = VertexFinder(entities)

        updatedEntities <- applyEnemyAttraction(
          entities,
          teams,
          vertexes,
          context.state.bounds.upperLeft,
          context.state.bounds.lowerRight,
          context.dt
        )
        updatedScene <- SceneEntitiesUpdate(context.state, updatedEntities)
      yield PhysicsRuleResult(updatedScene)

  /**
   * Traverses all mobile entities while preserving the first physics failure.
   *
   * @param entities
   *   current scene entities
   * @param teams
   *   current team relationships
   * @param vertexes
   *   pathfinding vertices indexed by entity identifier
   * @param upperLeftCorner
   *   upper-left world boundary
   * @param lowerRightCorner
   *   lower-right world boundary
   * @param dt
   *   elapsed nanoseconds
   * @return
   *   updated mobile entities, or the first [[PhysicsError]]
   */
  private def applyEnemyAttraction(
      entities: List[Entity],
      teams: List[Team],
      vertexes: Map[LocatableId, List[Vector2D]],
      upperLeftCorner: Vector2D,
      lowerRightCorner: Vector2D,
      dt: Long
  ): Either[PhysicsError, List[Entity]] = {
    val entitiesToMove = entities.filterNot(_.isFixed)

    entitiesToMove.foldLeft(Right(List.empty[Entity]): Either[PhysicsError, List[Entity]]):
      case (Left(err), _) => Left(err)
      case (Right(updatedEntities), entity) =>
        applyAttractionToEntity(
          entity,
          entities,
          teams,
          vertexes,
          upperLeftCorner,
          lowerRightCorner,
          dt
        ).map(updatedEntity => updatedEntities :+ updatedEntity)
  }

  /**
   * Steers one entity towards its nearest enemy when a reachable target exists.
   *
   * @param entity
   *   entity to orient
   * @param entities
   *   current scene entities
   * @param teams
   *   current team relationships
   * @param vertexes
   *   pathfinding vertices indexed by entity identifier
   * @param upperLeftCorner
   *   upper-left world boundary
   * @param lowerRightCorner
   *   lower-right world boundary
   * @param dt
   *   elapsed nanoseconds
   * @return
   *   oriented entity, unchanged entity when no target exists, or a [[PhysicsError]]
   */
  private[physics] def applyAttractionToEntity(
      entity: Entity,
      entities: List[Entity],
      teams: List[Team],
      vertexes: Map[LocatableId, List[Vector2D]],
      upperLeftCorner: Vector2D,
      lowerRightCorner: Vector2D,
      dt: Long
  ): Either[PhysicsError, Entity] =
    PhysicsUtil
      .nearestEnemy(entity, entities, teams)
      .fold(Right(entity): Either[PhysicsError, Entity]) { enemy =>
        RayCast(
          enemy,
          entity,
          entities,
          vertexes,
          upperLeftCorner,
          lowerRightCorner
        ).flatMap(
          _.fold(Right(entity): Either[PhysicsError, Entity]) { targetPosition =>
            entity.speed match
              case None =>
                Right(entity)
              case Some(currentSpeed) =>
                for seconds <- PhysicsUtil.timeLongToSeconds(dt)
                yield orientSpeed(
                  entity,
                  currentSpeed,
                  targetPosition,
                  MaxTurnRate * seconds
                )
          }
        )
      }

  /**
   * Rotates a velocity towards a target without changing its magnitude.
   * The signed angular difference is clamped to the maximum allowed turn.
   *
   * @param entity
   *   entity whose velocity is updated
   * @param currentSpeed
   *   current linear velocity
   * @param targetPosition
   *   desired world-space destination
   * @param maxTurnAngle
   *   maximum angular change in radians
   * @return
   *   entity with an oriented velocity, or unchanged when already at the target
   */
  private[physics] def orientSpeed(
      entity: Entity,
      currentSpeed: Vector2D,
      targetPosition: Vector2D,
      maxTurnAngle: Double
  ): Entity =
    val speedMagnitude = currentSpeed.magnitude

    val currentAngle    = math.atan2(currentSpeed.y, currentSpeed.x)
    val targetDirection = (targetPosition - entity.position).normalized

    if targetDirection.magnitude == 0 then entity
    else
      val targetAngle     = math.atan2(targetDirection.y, targetDirection.x)
      val angleDifference = normalizeAngle(targetAngle - currentAngle)
      val appliedTurn     = angleDifference.max(-maxTurnAngle).min(maxTurnAngle)
      val newAngle        = currentAngle + appliedTurn

      entity.withSpeed(
        Vector2D(
          math.cos(newAngle) * speedMagnitude,
          math.sin(newAngle) * speedMagnitude
        )
      )

  /**
   * Normalizes a radian angle to the interval from minus Pi to Pi.
   *
   * @param angle
   *   angle in radians
   * @return
   *   equivalent wrapped angle
   */
  private def normalizeAngle(angle: Double): Double =
    val twoPi = 2 * math.Pi
    ((angle + math.Pi) % twoPi + twoPi) % twoPi - math.Pi
