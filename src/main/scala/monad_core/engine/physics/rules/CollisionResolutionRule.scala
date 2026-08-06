package monad_core.engine.physics.rules

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.geometry.Collision
import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule}
import monad_core.engine.physics.utils.PhysicsUtil
import monad_core.engine.physics.utils.SceneUpdateEntity

private[physics] object CollisionResolutionRule:
  private val id = "collision-resolution"

  given collisionResolutionRule: PhysicsRule with

    override val ruleId: String = CollisionResolutionRule.id

    override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)
        entities = scene.allEntities
        
        activeCollisions = findCollisions(entities)
        
        updatedEntities <- resolveCollisions(entities, activeCollisions)

        updatedScene <-
          if activeCollisions.isEmpty then
            Right(scene)
          else
            SceneUpdateEntity.updateEntities(scene, updatedEntities)

      yield updatedScene

  private def findCollisions(entities: List[Entity])(using detector: CollisionDetector): Seq[(Entity, Entity, Collision)] =
    entities.combinations(2).flatMap {
      case Seq(e1, e2) =>
        if e1.speed.isDefined || e2.speed.isDefined then
          detector.collision(e1, e2).map(col => (e1, e2, col))
        else
          None
      case _ => None
    }.toSeq

  private def resolveCollisions(
                                 entities: List[Entity],
                                 collisions: Seq[(Entity, Entity, Collision)]
                               ): Either[PhysicsError,List[Entity]] =

    collisions
      .foldLeft(Right(entities.map(e => e.id -> e).toMap)
        : Either[PhysicsError,Map[LocatableId,Entity]]) {

        case (mapEither,(e1,e2,col)) =>

          mapEither.flatMap { map =>

            for
              (updated1,updated2) <-
                resolveCollision(
                  map(e1.id),
                  map(e2.id),
                  col
                )

            yield
              map
                .updated(updated1.id,updated1)
                .updated(updated2.id,updated2)
          }
      }
      .map(_.values.toList)

  private def resolveCollision(
                                e1: Entity,
                                e2: Entity,
                                collision: Collision
                              ): Either[PhysicsError, (Entity, Entity)] =

    (e1.isFixed, e2.isFixed) match

      // mobile vs fixed
      case (false, true) =>
        resolveMobileFixed(
          e1,
          e2,
          collision.normalVector,
          collision.penetrationDepth
        )

      // fixed vs mobile
      case (true, false) =>
        resolveMobileFixed(
          e2,
          e1,
          collision.normalVector * -1,
          collision.penetrationDepth
        )

      // mobile vs mobile
      case (false, false) =>
        resolveMobileMobile(
          e1,
          e2,
          collision.normalVector,
          collision.penetrationDepth
        )
        
      case (true, true) => 
        Right((e1, e2))

  private def resolveMobileFixed(
                                   mobile: Entity,
                                   fixed: Entity,
                                   collisionNormal: Vector2D,
                                   penetrationDepth: Double
                                 ): Either[PhysicsError, (Entity, Entity)] =
    for

      newPosition <- Right(
        PhysicsUtil.pushMobileOverlappingFixed(
          mobile.position,
          collisionNormal,
          penetrationDepth
        )
      )

      moved <- mobile
        .moveTo(newPosition)
        .left
        .map(PhysicsDomainError.apply)

      reflected <- moved.withSpeed(
        PhysicsUtil.reflectOnFixed(
          moved.speed.get,
          collisionNormal
        )
      )
      .left
      .map(PhysicsDomainError.apply)

    yield
      (reflected, fixed)

  private def resolveMobileMobile(
                                   e1: Entity,
                                   e2: Entity,
                                   collisionNormal: Vector2D,
                                   penetrationDepth: Double
                                 ): Either[PhysicsError, (Entity, Entity)] =
    for
      newPosition1 <- PhysicsUtil.pushMobileOverlappingMobile(
        e1.position,
        collisionNormal,
        penetrationDepth,
        e1.weight,
        e2.weight
      )

      newPosition2 <- PhysicsUtil.pushMobileOverlappingMobile(
        e2.position,
        collisionNormal * -1.0,
        penetrationDepth,
        e2.weight,
        e1.weight
      )

      moved1 <- e1
        .moveTo(newPosition1)
        .left
        .map(PhysicsDomainError.apply)

      moved2 <- e2
        .moveTo(newPosition2)
        .left
        .map(PhysicsDomainError.apply)

      speedReflected1 <- PhysicsUtil.reflectOnMobile(
        moved1.speed.get,
        moved2.speed.get,
        collisionNormal,
        moved1.weight,
        moved2.weight
      ).left.map(PhysicsDomainError.apply)

      reflected1 <- moved1.withSpeed(
        speedReflected1
      )
      .left
      .map(PhysicsDomainError.apply)

      speedReflected2 <- PhysicsUtil.reflectOnMobile(
        moved2.speed.get,
        moved1.speed.get,
        collisionNormal * -1.0,
        moved2.weight,
        moved1.weight
      ).left.map(PhysicsDomainError.apply)

      reflected2 <- moved2.withSpeed(
        speedReflected2
      )
      .left
      .map(PhysicsDomainError.apply)

    yield
      (reflected1, reflected2)