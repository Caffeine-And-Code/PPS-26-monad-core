package monad_core.simulator.infrastructure.engine.translators

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Vector2D, Surface as EngineSurface}
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.MonadCoreSurface

object SurfaceTranslator:
  extension (entity: EngineSurface)
    def toSimulationSurface: MonadCoreSurface =
      MonadCoreSurface(
        id = entity.id.value,
        position = (entity.position.x, entity.position.y),
        shape = BaseTranslator.determineShape(entity.shape),
        frictionIndex = entity.frictionIndex.flatMap(index => Option.apply(index)),
        appliedForce = entity.appliedForce.flatMap(force => Option.apply((force.x, force.y)))
      )

  extension (simulationSurface: MonadCoreSurface)
    def toEngineModel: Either[EngineError, EngineSurface] =
      def assignOptionalParamsToEntity(base: EngineSurface): Either[EngineError, EngineSurface] =
        for
          surfaceWithFriction <- simulationSurface.frictionIndex match
            case Some(index) => base.withFrictionIndex(index)
            case None => Right(base)

          finalSurface <- simulationSurface.appliedForce match
            case Some(force) => surfaceWithFriction.withAppliedForce(
              Vector2D(
                force._1,
                force._2
              )
            )
            case None => Right(surfaceWithFriction)

        yield finalSurface

      simulationSurface.shape match
        case SimulationCircle(radius) =>
          for
            base <- EngineSurface.circle(
              id = simulationSurface.id,
              position = Vector2D(
                simulationSurface.position._1,
                simulationSurface.position._2
              ),
              radius = radius
            )

            finalEntity <- assignOptionalParamsToEntity(base)

          yield finalEntity

        case SimulationRectangle(width, height) =>
          for
            base <- EngineSurface.rectangle(
              id = simulationSurface.id,
              position = Vector2D(
                simulationSurface.position._1,
                simulationSurface.position._2
              ),
              length = width,
              height = height
            )

            finalEntity <- assignOptionalParamsToEntity(base)

          yield finalEntity
