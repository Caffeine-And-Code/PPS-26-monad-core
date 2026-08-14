package monad_core.simulator.domain.engine

enum MonadCoreShape:
  case SimulationCircle(radius: Double)

  case SimulationRectangle(width: Double, height: Double)
