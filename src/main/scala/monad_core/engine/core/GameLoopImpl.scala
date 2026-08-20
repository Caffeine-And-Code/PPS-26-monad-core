package monad_core.engine.core

import monad_core.engine.core.GameLoop.StaticAlpha
import monad_core.engine.core.events.Event
import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.model.EngineError

import scala.annotation.tailrec

private case class FixedUpdateResult(
    updatedState: State,
    accumulator: Long,
    accumulatedEvents: Vector[Event]
)

private case class GameLoopImpl(
    mode: LoopMode,
    tickTime: Long,
    isRunning: Boolean,
    lastTime: Long,
    accumulator: Long,
    maxFrameTime: Long
) extends GameLoop:

  def withMode(newMode: LoopMode): GameLoop = this.copy(mode = newMode)

  def withTickTime(newTickTime: Long): Either[EngineError, GameLoop] =
    if newTickTime <= 0 then Left(InvalidTickTime(newTickTime))
    else if maxFrameTime < newTickTime then
      Left(InvalidMaxFrameTimeTickTimeRatio(maxFrameTime, newTickTime))
    else Right(this.copy(tickTime = newTickTime))

  def start(): GameLoop = this.copy(isRunning = true, mode = LoopMode.SimulationMode)
  def stop(): GameLoop  = this.copy(isRunning = false, mode = LoopMode.EditMode)

  def tick(state: State, currentTime: Long)(using
      physics: PhysicsEngine
  ): Either[EngineError, GameLoopTickResult] =
    if !isRunning || mode == LoopMode.EditMode then
      Right(
        GameLoopTickResult(
          state = state,
          loop = this.copy(lastTime = currentTime),
          events = Vector.empty,
          alpha = StaticAlpha
        )
      )
    else
      val elapsedTime   = currentTime - lastTime
      val clampedTime   = Math.min(elapsedTime, maxFrameTime)
      val remainingTime = accumulator + clampedTime
      val previousState = state

      for
        res <- runFixedUpdate(remainingTime, state, Vector.empty)
        alpha = res.accumulator.toDouble / tickTime.toDouble
      yield GameLoopTickResult(
        state = res.updatedState,
        loop = this.copy(lastTime = currentTime, accumulator = res.accumulator),
        events = res.accumulatedEvents,
        alpha = alpha
      )

  @tailrec
  private def runFixedUpdate(
      remainingTime: Long,
      currentScene: State,
      accumulatedEvents: Vector[Event]
  )(using physics: PhysicsEngine): Either[EngineError, FixedUpdateResult] =
    if remainingTime < tickTime then
      Right(
        FixedUpdateResult(
          currentScene,
          remainingTime,
          accumulatedEvents
        )
      )
    else
      physics.step(currentScene, tickTime) match
        case Left(err) => Left(err)
        case Right(step) =>
          runFixedUpdate(
            remainingTime - tickTime,
            step.state,
            accumulatedEvents ++ step.events
          )
