package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule, PhysicsRuleError}
import monad_core.engine.physics.pathfinding.{RayCast, VertexFinder}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

private[physics] object EnemyAttractionRule:
  private val Id          = "enemy-attraction"
  private val MaxTurnRate = 4.0 // radians per second

  given enemyAttractionRule: PhysicsRule with

    override val RuleId: String = EnemyAttractionRule.Id

    override def apply(scene: State, dt: Long)(using
        detector: CollisionDetector
    ): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities
        teams    = scene.allTeams

        vertexes = VertexFinder(entities)

        updatedEntities <- applyEnemyAttraction(
          entities,
          teams,
          vertexes,
          scene.bounds.upperLeft,
          scene.bounds.lowerRight,
          dt
        )
        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield updatedScene

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
            for
              currentSpeed <- entity.speed.toRight(
                PhysicsRuleError(
                  s"Entity ${entity.id} has no speed to apply enemy attraction"
                )
              )
              seconds <- PhysicsUtil.timeLongToSeconds(dt)
            yield orientSpeed(
              entity,
              currentSpeed,
              targetPosition,
              MaxTurnRate * seconds
            )
          }
        )
      }

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

  private def normalizeAngle(angle: Double): Double =
    val twoPi = 2 * math.Pi
    ((angle + math.Pi) % twoPi + twoPi) % twoPi - math.Pi
