package monad_core.simulator.application.engine.world

import monad_core.engine.core.Scene
import monad_core.engine.core.traits.State
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*

trait World extends TeamOperations, EntityOperations, SurfaceOperations:
  //TODO: The return type should be a domain entity, which will be a mapper to the State Entity 
  def scene: Scene