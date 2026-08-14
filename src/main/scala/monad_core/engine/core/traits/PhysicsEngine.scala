package monad_core.engine.core.traits

import monad_core.engine.errors.EngineError

private[engine] trait PhysicsEngine :
  def step(scene: State, dt: Long): Either[EngineError, State]
