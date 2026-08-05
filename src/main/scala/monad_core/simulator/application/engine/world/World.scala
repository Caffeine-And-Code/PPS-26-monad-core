package monad_core.simulator.application.engine.world

import monad_core.engine.core.Scene

trait World extends TeamOperations, EntityOperations, SurfaceOperations:
  //TODO: The return type should be a domain entity, which will be a mapper to the State Entity 
  def scene: Scene