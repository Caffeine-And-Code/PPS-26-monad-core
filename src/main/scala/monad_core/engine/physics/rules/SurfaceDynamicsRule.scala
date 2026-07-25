package monad_core.engine.physics.rules

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Surface, Vector2D}
import monad_core.engine.physics.core.{PhysicsDomainError, PhysicsError, PhysicsRule, PhysicsState, PhysicsUtil}

trait SurfaceDetection[CD]:
  def isInside(detector: CD, entity: Entity, surface: Surface): Boolean

object SurfaceDynamicsRule:

  given surfaceDynamicsRule[S, CD](using state: PhysicsState[S], surfaceDetection: SurfaceDetection[CD]): PhysicsRule[S, CD] with

    override def apply(scene: S)(using detector: CD, dt: Long): Either[PhysicsError, S] =
      for
        _ <- PhysicsUtil.deltaSeconds(dt)

        entities = state.getEntities(scene)
        surfaces = state.getSurfaces(scene)

        updatedScene <- entities.foldLeft[Either[PhysicsError, S]](Right(scene)):
          case (Left(err), _) => Left(err)
          case (Right(currentScene), (entityId, entity)) =>
            surfaces.values.foldLeft[Either[PhysicsError, Entity]](Right(entity)):
              case (Left(err), _) => Left(err)
              case (Right(currentEntity), surface) =>
                currentEntity.speed match
                  case None =>
                    Right(currentEntity)

                  case Some(_) =>
                    if surfaceDetection.isInside(detector, currentEntity, surface)
                    then applySurfaceDynamics(currentEntity, surface, dt)
                    else Right(currentEntity)
            .map: finalEntity =>
              if finalEntity == entity then
                currentScene
              else
                state.updateEntity(currentScene, entityId, finalEntity)
      yield updatedScene

    private def applySurfaceDynamics(entity: Entity, surface: Surface, dt: Long)(using CD): Either[PhysicsError, Entity] =
      entity.speed match
        case None =>
          Right(entity)

        case Some(speed) if surface.appliedForce.isEmpty && surface.frictionIndex.isEmpty =>
          Right(entity)

        case Some(speed) =>
          for
            speedAfterForce <-
              (surface.appliedForce, entity.weight) match
                case (Some(force), Some(weight)) =>
                  PhysicsUtil
                    .acceleration(force, Right(weight))
                    .left
                    .map(PhysicsDomainError.apply)
                    .flatMap(acc => PhysicsUtil.nextSpeed(speed, acc, dt))
                case _ =>
                  Right(speed)

            speedAfterFriction <-
              surface.frictionIndex.fold[Either[PhysicsError, Vector2D]](Right(speedAfterForce)): friction =>
                PhysicsUtil.applyFriction(speedAfterForce, friction, dt)

            updatedEntity <- entity
              .withSpeed(speedAfterFriction)
              .left
              .map(PhysicsDomainError.apply)
          yield updatedEntity