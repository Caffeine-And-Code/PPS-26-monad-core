package monad_core.engine.core

import monad_core.engine.core.traits.{PhysicsEngine, RenderEngine, State}
import monad_core.engine.model.EngineError
import monad_core.engine.simulator.Painter

trait GameLoop:
  def mode: LoopMode
  def tickTime: Long
  def isRunning: Boolean
  def lastTime: Long
  def accumulator: Long
  def maxFrameTime: Long

  def withMode(newMode: LoopMode): GameLoop
  def withTickTime(newTickTime: Long): Either[EngineError, GameLoop]
  def start(): GameLoop
  def stop(): GameLoop

  def tick(scene: State, currentTime: Long)(using
      physics: PhysicsEngine,
      painter: Painter
  ): Either[EngineError, (State, GameLoop)]

object GameLoop:
  val DefaultTickTime                 = 16_000_000L
  val InitialTime                     = 0L
  private val InitialAccumulatorValue = 0L
  val DefaultMaxFrameTime             = 250_000_000L
  val StaticAlpha                     = 1.0

  def apply(
      mode: LoopMode = LoopMode.EditMode,
      tickTime: Long = DefaultTickTime,
      isRunning: Boolean = false,
      lastTime: Long = InitialTime,
      accumulator: Long = InitialAccumulatorValue,
      maxFrameTime: Long = DefaultMaxFrameTime
  ): Either[EngineError, GameLoop] =
    for
      _ <- Either.cond(tickTime > 0, (), InvalidTickTime(tickTime))
      _ <- Either.cond(lastTime >= 0, (), InvalidLastTime(lastTime))
      _ <- Either.cond(accumulator >= 0, (), InvalidAccumulator(accumulator))
      _ <- Either.cond(maxFrameTime > 0, (), InvalidMaxFrameTime(maxFrameTime))
      _ <- Either.cond(
        maxFrameTime >= tickTime,
        (),
        InvalidMaxFrameTimeTickTimeRatio(maxFrameTime, tickTime)
      )
    yield GameLoopImpl(mode, tickTime, isRunning, lastTime, accumulator, maxFrameTime)

  def default(): GameLoop =
    GameLoopImpl(
      mode = LoopMode.EditMode,
      tickTime = DefaultTickTime,
      isRunning = false,
      lastTime = InitialTime,
      accumulator = InitialAccumulatorValue,
      maxFrameTime = DefaultMaxFrameTime
    )
