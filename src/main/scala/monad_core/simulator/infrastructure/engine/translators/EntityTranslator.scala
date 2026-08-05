package monad_core.simulator.infrastructure.engine.translators

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{LocatableId, Vector2D, Entity as EngineEntity, Shape2D as EngineShape}
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreShape}
import monad_core.simulator.errors.BaseError

private[infrastructure] object EntityTranslator:
  extension (entity: EngineEntity)
    def toSimulationEntity: MonadCoreEntity =
      MonadCoreEntity(
        id = entity.id.value,
        position = (entity.position.x, entity.position.y),
        shape = BaseTranslator.determineShape(entity.shape),
        speed = entity.speed.flatMap(speed => Option.apply(speed.x, speed.y)),
        health = entity.health.flatMap(health => Option.apply(health.value)),
        teamId = entity.teamId.flatMap(teamId => Option.apply(teamId.value))
      )

  extension (simulationEntity: MonadCoreEntity)
    def toEngineModel: Either[EngineError, EngineEntity] =
      def assignOptionalParamsToEntity(base: EngineEntity): Either[EngineError, EngineEntity] =
        for
          entityWithTeamId <- simulationEntity.teamId match
            case Some(teamId) => base.withTeamId(teamId)
            case None => Right(base)

          entityWithSpeed <- simulationEntity.speed match
            case Some(vector) => entityWithTeamId.withSpeed(Vector2D(vector._1, vector._2))
            case None => Right(entityWithTeamId)

          entityWithHealth <- simulationEntity.health match
            case Some(health) => entityWithSpeed.withHealth(health)
            case None => Right(entityWithSpeed)

          finalEntity <- simulationEntity.weight match
            case Some(weight) => entityWithHealth.withWeight(weight)
            case None => Right(entityWithHealth)

        yield finalEntity

      simulationEntity.shape match
        case SimulationCircle(radius) =>
          for
            base <- EngineEntity.circle(
              id = simulationEntity.id,
              position = Vector2D(
                simulationEntity.position._1,
                simulationEntity.position._2
              ),
              radius = radius
            )

            finalEntity <- assignOptionalParamsToEntity(base)

          yield finalEntity

        case SimulationRectangle(width, height) =>
          for
            base <- EngineEntity.rectangle(
              id = simulationEntity.id,
              position = Vector2D(
                simulationEntity.position._1,
                simulationEntity.position._2
              ),
              length = width,
              height = height
            )

            finalEntity <- assignOptionalParamsToEntity(base)

          yield finalEntity