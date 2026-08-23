package monad_core.engine.core.traits

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.model.EngineError

final private[engine] case class PhysicsStep(
    state: State,
    events: Vector[EngineEvent] = Vector.empty
)

private[engine] trait PhysicsEngine:
  def step(scene: State, dt: Long): Either[EngineError, PhysicsStep]
