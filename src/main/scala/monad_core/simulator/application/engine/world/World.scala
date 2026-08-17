package monad_core.simulator.application.engine.world

import monad_core.engine.model.Scene
import monad_core.simulator.errors.BaseError

trait World extends TeamOperations, EntityOperations, SurfaceOperations:
  def scene: Scene

  def resize(width: Double, height: Double): Either[BaseError, Unit]
