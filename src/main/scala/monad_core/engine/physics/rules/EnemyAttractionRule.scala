package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule}
import monad_core.engine.physics.pathfinding.{RayCast, VertexFinder}
import monad_core.engine.physics.utils.{PhysicsUtil, SceneEntitiesUpdate}

private[physics] object EnemyAttractionRule:
  private val Id = "enemy-attraction"
  private val AttractionAcceleration = 1.0

  given enemyAttractionRule: PhysicsRule with

    override val RuleId: String = EnemyAttractionRule.Id

    override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.timeLongToSeconds(dt)
        entities = scene.allEntities
        teams = scene.allTeams
        
        vertexes = VertexFinder(entities)
        
        updatedEntities <- applyEnemyAttraction(
          entities,
          teams,
          vertexes,
          scene.UpperLeftCorner,
          scene.LowerRightCorner
        )
        updatedScene <- SceneEntitiesUpdate(scene, updatedEntities)
      yield updatedScene
      
    private def applyEnemyAttraction(
                                      entities: List[Entity],
                                      teams: List[Team],
                                      vertexes: Map[LocatableId, List[Vector2D]],
                                      upperLeftCorner: Vector2D,
                                      lowerRightCorner: Vector2D,
                                    ): Either[PhysicsError, List[Entity]] = {
      val entitiesToMove = entities.filterNot(_.isFixed)

      entitiesToMove.foldLeft(Right(List.empty[Entity]): Either[PhysicsError, List[Entity]]) :
        case (Left(err), _) => Left(err)
        case (Right(updatedEntities), entity) =>
          
          applyAttractionToEntity(entity, entities, teams, vertexes, upperLeftCorner, lowerRightCorner)
            .map { updatedEntity => updatedEntities :+ updatedEntity }
    }

    private def applyAttractionToEntity(
                                 entity: Entity,
                                 entities: List[Entity],
                                 teams: List[Team],
                                 vertexes: Map[LocatableId, List[Vector2D]],
                                 upperLeftCorner: Vector2D,
                                 lowerRightCorner: Vector2D
                               ): Either[PhysicsError, Entity] =
      val nearestEnemy = PhysicsUtil.nearestEnemy(
        entity, entities, teams
      )
      
      nearestEnemy match
        case Some(enemy) =>
          val targetPosition = RayCast(
            enemy,
            entity,
            entities,
            vertexes,
            upperLeftCorner,
            lowerRightCorner
          )

          targetPosition match
            case Left(err) => Left(err)
            case Right(Some(targetPos)) =>
              val direction = (targetPos - entity.position).normalized
              val speed = direction * AttractionAcceleration

              Right(entity.withSpeed(entity.speed.get + speed))
            case Right(None) =>
              Right(entity)
          
        case None =>
          Right(entity)
      