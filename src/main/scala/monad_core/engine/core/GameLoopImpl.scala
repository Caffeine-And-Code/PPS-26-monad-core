package monad_core.engine.core

import monad_core.engine.core.GameLoop.StaticAlpha
import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.model.EngineError

import scala.annotation.tailrec

/**
 * Result of consuming all complete fixed updates available in an elapsed-time budget.
  *
  * @param previousState
  *   state preceding the last completed physics update
  * @param updatedState
  *   state produced by the last completed physics update
  * @param accumulator
  *   unconsumed time in nanoseconds
  * @param accumulatedEvents
  *   events produced by all completed updates
  */
private case class FixedUpdateResult(
    previousState: State,
    updatedState: State,
    accumulator: Long,
    accumulatedEvents: Vector[EngineEvent]
)

/**
  * Immutable implementation of a fixed-step game loop.
  *
  * @param mode
  *   current editing or simulation mode
  * @param tickTime
  *   duration of one fixed physics update in nanoseconds
  * @param isRunning
  *   whether time advancement is enabled
  * @param lastTime
  *   timestamp received by the previous loop update
  * @param accumulator
  *   elapsed time retained for subsequent fixed updates
  * @param maxFrameTime
  *   maximum elapsed time accepted from one rendered frame
  */
private case class GameLoopImpl(
    mode: LoopMode,
    tickTime: Long,
    isRunning: Boolean,
    lastTime: Long,
    accumulator: Long,
    maxFrameTime: Long
) extends GameLoop:

  /**
    * Returns a loop using the supplied execution mode.
    *
    * @param newMode
    *   replacement mode
    * @return
    *   updated immutable loop
    */
  def withMode(newMode: LoopMode): GameLoop = this.copy(mode = newMode)

  /**
    * Returns a loop using a validated positive tick duration.
    *
    * @param newTickTime
    *   replacement tick duration in nanoseconds
    * @return
    *   updated loop, or an invalid-tick or frame-ratio error
    */
  def withTickTime(newTickTime: Long): Either[EngineError, GameLoop] =
    if newTickTime <= 0 then Left(InvalidTickTime(newTickTime))
    else if maxFrameTime < newTickTime then
      Left(InvalidMaxFrameTimeTickTimeRatio(maxFrameTime, newTickTime))
    else Right(this.copy(tickTime = newTickTime))

  /**
    * Starts time advancement in simulation mode.
    *
    * @return
    *   running form of this loop
    */
  def start(): GameLoop = this.copy(isRunning = true, mode = LoopMode.SimulationMode)

  /**
    * Stops time advancement and set edit mode.
    *
    * @return
    *   stopped form of this loop
    */
  def stop(): GameLoop  = this.copy(isRunning = false, mode = LoopMode.EditMode)

  /**
    * Advances the loop using every fixed physics update that fits in the accumulated time.
    * Elapsed time is clamped to the frame limit and the remainder determines the interpolation ratio.
    *
    * @param state
    *   state from which the update starts
    * @param currentTime
    *   current timestamp in nanoseconds
    * @param physics
    *   physics engine used for each fixed update
    * @return
    *   tick result containing the updated loop, states and events, or a physics error
    */
  def tick(state: State, currentTime: Long)(using
      physics: PhysicsEngine
  ): Either[EngineError, GameLoopTickResult] =
    if !isRunning || mode == LoopMode.EditMode then
      Right(
        GameLoopTickResult(
          previousState = state,
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

      for
        res <- runFixedUpdate(remainingTime, state, state, Vector.empty)
        alpha = res.accumulator.toDouble / tickTime.toDouble
      yield GameLoopTickResult(
        previousState = res.previousState,
        state = res.updatedState,
        loop = this.copy(lastTime = currentTime, accumulator = res.accumulator),
        events = res.accumulatedEvents,
        alpha = alpha
      )

  /**
    * Recursively consumes complete fixed ticks and retains the final sub-tick remainder.
    * The previous state follows the last completed transition for render interpolation.
    *
    * @param remainingTime
    *   elapsed time still available in nanoseconds
    * @param previousState
    *   state preceding the latest completed physics update
    * @param currentState
    *   state from which the next physics update starts
    * @param accumulatedEvents
    *   events produced by the updates already completed
    * @param physics
    *   physics engine used for each fixed update
    * @return
    *   completed update data, or the first physics error
    */
  @tailrec
  private def runFixedUpdate(
      remainingTime: Long,
      previousState: State,
      currentState: State,
      accumulatedEvents: Vector[EngineEvent]
  )(using physics: PhysicsEngine): Either[EngineError, FixedUpdateResult] =
    if remainingTime < tickTime then
      Right(
        FixedUpdateResult(
          previousState = previousState,
          updatedState = currentState,
          accumulator = remainingTime,
          accumulatedEvents = accumulatedEvents
        )
      )
    else
      physics.step(currentState, tickTime) match
        case Left(err) => Left(err)
        case Right(step) =>
          runFixedUpdate(
            remainingTime - tickTime,
            currentState,
            step.state,
            accumulatedEvents ++ step.events
          )
