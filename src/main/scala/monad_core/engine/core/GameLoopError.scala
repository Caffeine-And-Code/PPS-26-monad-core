package monad_core.engine.core

import monad_core.engine.errors.EngineError

sealed abstract class GameLoopError(message: String) extends EngineError(message)

case class InvalidTickTime(tickTime: Long)
  extends GameLoopError(s"Tick time cannot be negative or zero: $tickTime")

case class InvalidLastTime(lastTime: Long)
  extends GameLoopError(s"Last time cannot be negative: $lastTime")

case class InvalidAccumulator(accumulator: Long)
  extends GameLoopError(s"Accumulator cannot be negative: $accumulator")

case class InvalidMaxFrameTime(maxFrameTime: Long)
  extends GameLoopError(s"Max frame time cannot be negative or zero: $maxFrameTime")

case class InvalidMaxFrameTimeTickTimeRatio(maxFrameTime: Long, tickTime: Long)
  extends GameLoopError(s"Max frame time ($maxFrameTime) cannot be less than tick time ($tickTime)")