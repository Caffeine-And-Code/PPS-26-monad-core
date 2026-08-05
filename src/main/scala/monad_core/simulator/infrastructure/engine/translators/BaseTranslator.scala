package monad_core.simulator.infrastructure.engine.translators

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity as EngineEntity, Shape2D as EngineShape, Surface as EngineSurface}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreShape, MonadCoreSurface}
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.infrastructure.engine.translators.EntityTranslator.toSimulationEntity
import monad_core.simulator.infrastructure.engine.translators.SurfaceTranslator.toSimulationSurface

object BaseTranslator:
  def determineShape(shape2D: EngineShape): MonadCoreShape =
    shape2D match
      case EngineShape.Circle(radius) => SimulationCircle(radius)
      case EngineShape.Rectangle(length, height) => SimulationRectangle(length, height)

  extension (either: Either[EngineError, EngineEntity])
    def toSimulationEitherEntity: Either[EngineError, MonadCoreEntity] =
      either.map(_.toSimulationEntity)

  extension (either: Either[EngineError, EngineSurface])
    def toSimulationEitherSurface: Either[EngineError, MonadCoreSurface] =
      either.map(_.toSimulationSurface)