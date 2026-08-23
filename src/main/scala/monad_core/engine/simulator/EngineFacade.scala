package monad_core.engine.simulator

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.State
import monad_core.engine.core.{GameLoop, LoopMode}
import monad_core.engine.model.EngineError
import monad_core.engine.physics.core.PhysicsManager

object EngineFacade:

  opaque type Session = GameLoop

  val DefaultTickTime: Long     = GameLoop.DefaultTickTime
  val DefaultMaxFrameTime: Long = GameLoop.DefaultMaxFrameTime

  final case class TickResult(
      state: State,
      nextSession: Session,
      events: Vector[EngineEvent],
      alpha: Double
  )

  def default: Session =
    GameLoop.default()

  def create(
      tickTime: Long = DefaultTickTime,
      maxFrameTime: Long = DefaultMaxFrameTime
  ): Either[EngineError, Session] =
    GameLoop(tickTime = tickTime, maxFrameTime = maxFrameTime)

  def start(session: Session): Session =
    session.start()

  def stop(session: Session): Session =
    session.stop()

  def mode(session: Session): LoopMode =
    session.mode

  def isRunning(session: Session): Boolean =
    session.isRunning

  def tick(
      session: Session,
      state: State,
      currentTime: Long,
      physics: PhysicsManager = PhysicsManager.default()
  ): Either[EngineError, TickResult] =
    given PhysicsManager = physics

    session.tick(state, currentTime).map { result =>
      TickResult(
        state = result.state,
        nextSession = result.loop,
        events = result.events,
        alpha = result.alpha
      )
    }
