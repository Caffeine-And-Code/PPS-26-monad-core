package monad_core.engine.physics.rules

import monad_core.engine.model.{Entity, LocatableId, Vector2D, dot, sub, times}
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule, PhysicsState, PhysicsUtil}

trait Collision:
  val normalVector: Vector2D
  val penetrationDepth: Double

trait CollisionResolutionDetection[CD]:
  def collision(detector: CD, first: Entity, second: Entity): Option[Collision]

object CollisionResolutionRule:
  private val id = "collision-resolution"
  
  given collisionResolutionRule[S, CD](using state: PhysicsState[S], collisionDetection: CollisionResolutionDetection[CD]): PhysicsRule[S, CD] with
    
    override val ruleId: String = CollisionResolutionRule.id
    
    override def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)
        entities = state.getEntities(scene)

        updatedScene <- entities.foldLeft[Either[PhysicsError, S]](Right(scene)):
          case (Left(err), _) => Left(err)

          case (Right(currentScene), (entityId, entity)) =>
            entity.speed match
              case None =>
                Right(currentScene)

              case Some(_) =>
                entities.values
                  .filterNot(_.id == entityId)
                  .foldLeft[Either[PhysicsError, Entity]](Right(entity)):
                    case (Left(err), _) => Left(err)

                    case (Right(currEnt), otherEnt) =>
                      collisionDetection.collision(detector, currEnt, otherEnt) match
                        case None => Right(currEnt)
                        case Some(collision) => resolveCollision(currEnt, collision)

                  .flatMap(updatedEntity =>
                    if updatedEntity == entity then Right(currentScene)
                    else Right(state.updateEntity(currentScene, entityId, updatedEntity))
                  )
      yield updatedScene

    private def resolveCollision(entity: Entity, collision: Collision): Either[PhysicsError, Entity] =
      entity.speed match
        case None => Right(entity)
        case Some(speed) =>
          val speedAlongNormal = speed dot collision.normalVector

          if speedAlongNormal >= 0 then
            Right(entity)
          else
            entity
              .withSpeed(
                speed sub (collision.normalVector times (2 * speedAlongNormal))
              )
              .left
              .map(PhysicsDomainError.apply)