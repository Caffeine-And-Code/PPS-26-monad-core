package monad_core.engine.core

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.model.EngineError

final case class GameLoopTickResult(
    state: State,
    previousState: State,
    loop: GameLoop,
    events: Vector[EngineEvent],
    alpha: Double
)

/** Immutable control and timing state of the simulation loop. */
trait GameLoop:
  /**
   * Returns the current mode between edit or simulation.
   *
   * @return
   *   current loop mode
   */
  def mode: LoopMode

  /**
   * Returns the duration of one fixed physics tick.
   *
   * @return
   *   tick duration in nanoseconds
   */
  def tickTime: Long

  /**
   * Reports whether time advancement is enabled.
   *
   * @return
   *   `true` when the loop is running
   */
  def isRunning: Boolean

  /**
   * Returns the timestamp received by the previous loop update.
   *
   * @return
   *   previous timestamp in nanoseconds
   */
  def lastTime: Long

  /**
   * Returns elapsed time retained for subsequent fixed ticks.
   *
   * @return
   *   unconsumed nanoseconds
   */
  def accumulator: Long

  /**
   * Returns the maximum elapsed time accepted from one rendered frame.
   *
   * @return
   *   maximum frame duration in nanoseconds
   */
  def maxFrameTime: Long

  /**
   * Returns a loop using the supplied execution mode.
   *
   * @param newMode
   *   replacement mode
   * @return
   *   updated immutable loop
   */
  def withMode(newMode: LoopMode): GameLoop

  /**
   * Returns a loop using a validated positive tick duration.
   *
   * @param newTickTime
   *   replacement tick duration in nanoseconds
   * @return
   *   updated loop, or an invalid-tick or frame-ratio error
   */
  def withTickTime(newTickTime: Long): Either[EngineError, GameLoop]

  /**
   * Starts time advancement.
   *
   * @return
   *   running form of this loop
   */
  def start(): GameLoop

  /**
   * Stops time advancement.
   *
   * @return
   *   stopped form of this loop
   */
  def stop(): GameLoop

  def tick(scene: State, currentTime: Long)(using
      physics: PhysicsEngine
  ): Either[EngineError, GameLoopTickResult]

/** Validated constructors and defaults for a fixed-step game loop. */
object GameLoop:
  /** Default fixed tick duration in nanoseconds. */
  val DefaultTickTime = 16_000_000L

  /** Timestamp used before the first loop update. */
  val InitialTime = 0L

  /** Accumulated time used by a newly created loop. */
  private val InitialAccumulatorValue = 0L

  /** Default upper bound for elapsed frame time in nanoseconds. */
  val DefaultMaxFrameTime = 250_000_000L

  /** Interpolation ratio used while the loop is not simulating. */
  val StaticAlpha = 1.0

  /**
   * Creates a loop after validating all timing values.
   *
   * @param mode
   *   initial editing or simulation mode
   * @param tickTime
   *   positive fixed-tick duration in nanoseconds
   * @param isRunning
   *   initial running state
   * @param lastTime
   *   non-negative previous timestamp
   * @param accumulator
   *   non-negative unconsumed elapsed time
   * @param maxFrameTime
   *   positive maximum frame duration not shorter than one tick
   * @return
   *   the validated loop, or the first invalid timing error
   */
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

  /**
   * Creates the default editing-mode loop without repeating validation.
   *
   * @return
   *   stopped loop using all default timing values
   */
  def default(): GameLoop =
    GameLoopImpl(
      mode = LoopMode.EditMode,
      tickTime = DefaultTickTime,
      isRunning = false,
      lastTime = InitialTime,
      accumulator = InitialAccumulatorValue,
      maxFrameTime = DefaultMaxFrameTime
    )
