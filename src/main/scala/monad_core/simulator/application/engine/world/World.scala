package monad_core.simulator.application.engine.world

import monad_core.simulator.domain.engine.MonadCoreScene

trait World extends TeamOperations, EntityOperations, SurfaceOperations:
  def scene: MonadCoreScene
