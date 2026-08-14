package monad_core.simulator.application.engine.world

import monad_core.engine.model.Scene

trait World extends TeamOperations, EntityOperations, SurfaceOperations:
  def scene: Scene
