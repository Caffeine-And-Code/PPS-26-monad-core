package monad_core.engine.core

import monad_core.engine.model.EngineError

/** Base type for invalid game-loop timing configurations. */
sealed abstract class GameLoopError(message: String) extends EngineError(message)

/** Indicates that the fixed tick duration is not positive. */
case class InvalidTickTime(tickTime: Long)
    extends GameLoopError(s"Tick time cannot be negative or zero: $tickTime")

/** Indicates that the previous loop timestamp is negative. */
case class InvalidLastTime(lastTime: Long)
    extends GameLoopError(s"Last time cannot be negative: $lastTime")

/** Indicates that the retained elapsed-time accumulator is negative. */
case class InvalidAccumulator(accumulator: Long)
    extends GameLoopError(s"Accumulator cannot be negative: $accumulator")

/** Indicates that the maximum accepted frame duration is not positive. */
case class InvalidMaxFrameTime(maxFrameTime: Long)
    extends GameLoopError(s"Max frame time cannot be negative or zero: $maxFrameTime")

/** Indicates that the maximum frame duration is shorter than one fixed tick. */
case class InvalidMaxFrameTimeTickTimeRatio(maxFrameTime: Long, tickTime: Long)
    extends GameLoopError(
      s"Max frame time ($maxFrameTime) cannot be less than tick time ($tickTime)"
    )
